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
#include <boost/noncopyable.hpp>
#include <boost/compatibility/cpp_c_headers/cerrno>
#include <boost/compatibility/cpp_c_headers/csignal>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>

#include <boost/scoped_ptr.hpp>
#include <iostream>

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

static rc_t release_pid(pid_t & pid)
{
  if (pid == 0 || pid == -1) return pid;
  BOOST_ASSERT(pid > 0);
  kill(pid, SIGTERM);
  int status;
  waitpid(pid, &status, 0);
  pid = -1;
  return status;
}

static bool set_blocking(fd_t fd, bool enable)
{
  int rc( fcntl(fd, F_GETFL, 0) );
  if (rc == -1) return false;
  rc = enable ? (rc | O_NONBLOCK) : (rc & ~O_NONBLOCK);
  rc = fcntl(fd, F_SETFL, rc);
  return rc != -1;
}

static bool make_pipe(read_fd_t & rend, write_fd_t & wend)
{
  fd_t fds[2];
  if (pipe(fds) != 0) return false;
  rend = fds[0];
  wend = fds[1];
  return true;
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

    char const * what( "impossible failure in hook construction" );

    read_fd_t  child_in( -1 );
    write_fd_t child_out( -1 );
    write_fd_t child_err( -1 );

    if (  !(make_pipe(child_in, _in)   /* && set_blocking(_in,  false) */)
       || !(make_pipe(_out, child_out) /* && set_blocking(_out, false) */)
       || !(make_pipe(_err, child_err) /* && set_blocking(_err, false) */)
       )
    {
      what = "failed to create pipe to sub-process";
      goto error_exit;
    }

    char ** my_env( environ );
    environ = env;

    _pid = fork();
    switch(_pid)
    {
      case 0:                   // Child: close parent's end of the pipes
        kill();                 // and use our ends for stdio.
        if (  dup2(child_in,  STDIN_FILENO)  != STDIN_FILENO
           || dup2(child_out, STDOUT_FILENO) != STDOUT_FILENO
           || dup2(child_err, STDERR_FILENO) != STDERR_FILENO
           )
        {
          MSG_ERROR("sub-process '" << cmd << "' cannot use pipes: " << system_error().what());
          goto error_exit;
        }
        release(child_in); release(child_out); release(child_err);
        execv(cmd, argv);
        MSG_ERROR("sub-process cannot execute '" << cmd << "': " << system_error().what());
        goto error_exit;

      case -1:                  // Error.
        environ = my_env;
        what = "cannot fork";
        goto error_exit;

      default:                  // Child spawned successfully.
        environ = my_env;
        release(child_in); release(child_out); release(child_err);
        return;
    }

  error_exit:
    release(child_in); release(child_out); release(child_err);
    kill();
    if (environ == my_env)      throw system_error(string("hook '") + _id + "' " + what);
    else                        exit(1);
  }

  ~hook()
  {
    kill();
  }

  rc_t kill()
  {
    release(_in); release(_out); release(_err);
    return release_pid(_pid);
  }

  rc_t commit()
  {
    release(_in);
    if (_pid != 0 && _pid != -1)
    {
      rc_t status;
      waitpid(_pid, &status, 0);
      _pid = -1;
      return status;
    }
    else
      return _pid;
  }
};

// void feed(char const * b, char const * e)
// {
//   BOOST_ASSERT(_id >= 0);
//   BOOST_ASSERT(b < e);
//   size_t len( e - b );
//   ssize_t const rc( write(_fd, b, len) );
//   if (rc <= 0)           throw system_error(string("feeding target '") + _id + "' failed");
//   if (size_t(rc) != len) throw logic_error(string("unexpected short write on target '") + _id + "'");
// }

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

int main(int, char ** argv)
{
  boost::scoped_ptr<hook> f( new hook(*(++argv), argv) );
  rc_t const status( f->commit() );
  if (WIFEXITED(status))
  {
    cout << "hook returned status " << WEXITSTATUS(status) << endl;
  }
  else
  {
    cout << "hook "
         << ( WIFSIGNALED(status) ? "received a signal"
            : WCOREDUMP(status)   ? "dumped core"
            : "was terminated for whatever OS-specific reason"
            )
         << endl;
  }

  cout << "*** output follows ..." << endl;
  slurp(f->_out, cout);
  cout << endl;

  cout << "*** errors follow ..." << endl;
  slurp(f->_err, cout);
  cout << endl;

  return 0;
}
