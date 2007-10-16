/*
 * Copyright (c) 2007 Peter Simons <simons@cryp.to>
 *
 * This software is provided 'as-is', without any express or
 * implied warranty. In no event will the authors be held liable
 * for any damages arising from the use of this software.
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty
 * provided the copyright notice and this notice are preserved.
 */

#include "postmaster.hpp"
#include "target.hpp"
#include "ioxx/probe.hpp"
#include "ioxx/timeout.hpp"
#include <sstream>
#include <boost/compatibility/cpp_c_headers/cerrno>
#include <boost/bind.hpp>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#include <boost/test/included/prg_exec_monitor.hpp>
#include <boost/scoped_ptr.hpp>

using namespace std;

typedef int     rc_t;
typedef int     fd_t;
typedef fd_t    read_fd_t;
typedef fd_t    write_fd_t;

static bool was_interrupted()   { return errno == EINTR; }
static bool would_block()       { return errno == EAGAIN || errno == EWOULDBLOCK; }

static void release(fd_t & fd)
{
  if (fd < 0) return;
  size_t again( 3u );
 try_again:
  if (close(fd) != 0 && was_interrupted() && --again)
    goto try_again;
  fd = -1;
}

static bool set_blocking(fd_t fd, bool enable)
{
  int const flags( fcntl(fd, F_GETFL, 0) );
  if (flags == -1) return false;
  int const new_flags( enable ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK) );
  return flags == new_flags ? true : fcntl(fd, F_SETFL, new_flags) != -1;
}

static bool make_pipe(read_fd_t & rend, write_fd_t & wend)
{
  fd_t fds[2];
  if (pipe(fds) != 0) return false;
  rend = fds[0];
  wend = fds[1];
  return true;
}

static std::ostream & print_child_rc_t(std::ostream & os, rc_t status)
{
  return WIFEXITED(status)
    ? os << "hook returned status " << WEXITSTATUS(status)
    : os << "hook "
         << ( WIFSIGNALED(status) ? "received a signal"
            : WCOREDUMP(status)   ? "dumped core"
            : "was terminated for whatever OS-specific reason"
            )
    ;
}

struct hook : private boost::noncopyable
{
  string const  _id;
  write_fd_t    _in;
  read_fd_t     _out;
  read_fd_t     _err;
  pid_t         _pid;

  hook(char const * cmd, char ** argv, char ** env = 0)
  : _id(cmd), _in(-1), _out(-1), _err(-1), _pid(-1)
  {
    BOOST_ASSERT(cmd);
    char ** const my_env( environ );

    char const * what( "impossible failure in hook construction" );

    read_fd_t  child_in( -1 );
    write_fd_t child_out( -1 );
    write_fd_t child_err( -1 );

    if (  !(make_pipe(child_in, _in)   && set_blocking(_in,  true)) /// \todo Hmm.
       || !(make_pipe(_out, child_out) && set_blocking(_out, true))
       || !(make_pipe(_err, child_err) && set_blocking(_err, true))
       )
    {
      what = "failed to create pipe to sub-process";
      goto error_exit;
    }

    _pid = fork();
    switch(_pid)
    {
      case 0:                   // Child: Set designated environment for
        environ = env;          // upcoming exec(). Then close parent's end of
        kill();                 // the pipes and use our ends for stdio.
        if (  dup2(child_in,  STDIN_FILENO)  != STDIN_FILENO
           || dup2(child_out, STDOUT_FILENO) != STDOUT_FILENO
           || dup2(child_err, STDERR_FILENO) != STDERR_FILENO
           )
        {
          what = "cannot use pipes";
          goto error_exit;
        }
        release(child_in); release(child_out); release(child_err);
        execv(cmd, argv);
        what = "cannot execute";
        goto error_exit;

      case -1:                  // Error.
        what = "cannot fork";
        goto error_exit;

      default:                  // Child spawned successfully.
        release(child_in); release(child_out); release(child_err);
        return;
    }

  error_exit:
    release(child_in); release(child_out); release(child_err);
    kill();
    system_error err( string("hook '") + _id + "' " + what );
    if (environ == my_env) throw err;
    MSG_ERROR("sub " << err.what());
    exit(1);
  }

  ~hook()
  {
    kill();
  }

  void kill()
  {
    release(_in); release(_out); release(_err);
    if (_pid > 0)
    {
      if (::kill(_pid, SIGKILL) == 0) // TODO: handle EINTR
      {
        rc_t status;
        waitpid(_pid, &status, 0);
      }
      _pid = -1;
    }
  }

  void commit()
  {
    release(_in);
    BOOST_ASSERT(_pid > 0);
    rc_t status;
    if (waitpid(_pid, &status, 0) == -1)
      throw system_error(string("cannot commit hook ") + _id);
    _pid = -1;
    if (status != 0)
    {
      ostringstream strbuf;
      strbuf << "target " << _id << ": ";
      print_child_rc_t(strbuf, status);
      throw runtime_error(strbuf.str());
    }
  }

  void feed(char const * b, char const * e)
  {
    BOOST_ASSERT(b < e);
    size_t len( e - b );
    ssize_t const rc( write(_in, b, len) );
    if (rc <= 0)           throw system_error(string("feeding hook '") + _id + "' failed");
    if (size_t(rc) != len) throw logic_error(string("unexpected short write on hook '") + _id + "'");
  }
};

bool slurp(read_fd_t fd, std::ostream & os)
{
  char    buf[1024 * 4];
  ssize_t rc;
  for (;;)
  {
    rc = read(fd, buf, sizeof(buf));
    if (rc == 0) return false;
    if (rc <  0) return would_block();
    os.write(buf, rc);
  }
}

struct async_hook : public ioxx::socket
{
  hook          _hook;

  typedef boost::shared_ptr<async_hook> pointer;

  async_hook(char const * cmd, char ** argv, char ** env = 0) : _hook(cmd, argv, env)
  {
  }
  bool input_blocked(read_fd_t fin) const
  {
    return ioxx::is_valid_weak_socket(_hook._in);
  }
  bool output_blocked(write_fd_t fout) const
  {
    return false;
  }
  void unblock_input(ioxx::socket::probe & p, read_fd_t fin)
  {
    throw logic_error("unblock_input not implemented");
  }
  void unblock_output(ioxx::socket::probe & p, write_fd_t fout)
  {
    throw logic_error("unblock_output not implemented");
  }
  void shutdown(ioxx::socket::probe & p, fd_t fd)
  {
    shutdown(p);
  }
  void shutdown(ioxx::socket::probe & p)
  {
    p.remove(_hook._in);  p.remove(STDIN_FILENO);
    p.remove(_hook._out); p.remove(STDOUT_FILENO);
    p.remove(_hook._err); p.remove(STDERR_FILENO);
    _hook.kill();
  }
};


int cpp_main(int, char ** argv)
{
  using namespace boost;

  // create i/o dispatcher
  scoped_ptr<ioxx::socket::probe> probe( ioxx::socket::probe::make() );
  ioxx::timeout timer;

  // start hook
  {
    char const * user_env[] = { "TERM=dumb", 0 };
    // The cast is dubious but probably safe based on the assumption that the
    // hook doesn't use that environment for anything but passing it to exec().
    async_hook::pointer f( new async_hook( *(++argv), argv, const_cast<char**>(user_env)) );
    probe->insert(STDIN_FILENO, f);
    probe->insert(STDOUT_FILENO, f);
    probe->insert(STDERR_FILENO, f);
    probe->insert(f->_hook._in, f);
    probe->insert(f->_hook._out, f);
    probe->insert(f->_hook._err, f);
    timer.in(5u, boost::bind<void>(&async_hook::shutdown, f, boost::ref(*probe)));
  }

  // run i/o loop
  for(;;)
  {
    ioxx::second_t idle_time;
    timer.deliver(&idle_time);
    if (probe->empty()) break;
    if (probe->run_once(timer.empty() ? -1 : static_cast<int>(idle_time)) == 0u)
      cout << "probe timeout" << endl;
  }

//   ostringstream strbuf;
//   slurp(STDIN_FILENO, strbuf);
//   string const & buf( strbuf.str() );
//   cout << "pipe " << buf.size() << " bytes from stdin" << endl;
//   write(f->_in, buf.c_str(), buf.size());
//
//   rc_t status( f->commit() );
//   print_child_rc_t(cout, status) << endl;
//
//   cout << "*** output follows ..." << endl;
//   slurp(f->_out, cout);
//   cout << endl;
//
//   cout << "*** errors follow ..." << endl;
//   slurp(f->_err, cout);
//   cout << endl;

  return 0;
}
