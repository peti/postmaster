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
#include <boost/compatibility/cpp_c_headers/cerrno>
#include <boost/compatibility/cpp_c_headers/csignal>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>

using namespace std;

live_target::~live_target()
{
}

static void release(string const & id, int & fd)
{
  if (fd >= 0)
  {
    if (close(fd) != 0)
    {
      if (errno == EINTR)
      {
        if (close(fd) != 0)
          goto bad;
      }
      else
        goto bad;
    }
  }
  fd = -1;
    return;

 bad:
    throw system_error(string("cannot close target '") + id + "'");
}

class fd_target : public live_target
{
protected:
  string _id;
  int    _fd;

public:
  fd_target(string const & id, int fd) : _id(id), _fd(fd)
  {
    BOOST_ASSERT(_fd >= 0);
    BOOST_ASSERT(!_id.empty());
    flock lk;
    memset(&lk, 0, sizeof(lk));
    lk.l_type   = F_WRLCK;
    lk.l_whence = SEEK_CUR;
    int const rc( fcntl(_fd, F_SETLK, &lk) );
    if (rc != 0)
    {
      release(_id, _fd);
      throw system_error(string("cannot lock target '" + _id + "'"));
    }
    MSG_TRACE("target " << _id << " locked successfully");
  }

  ~fd_target()
  {
    release(_id, _fd);
  }

  void feed(char const * b, char const * e)
  {
    BOOST_ASSERT(_fd >= 0);
    BOOST_ASSERT(b < e);
    size_t len( e - b );
    ssize_t const rc( write(_fd, b, len) );
    if (rc <= 0)           throw system_error(string("feeding target '") + _id + "' failed");
    if (size_t(rc) != len) throw logic_error(string("unexpected short write on target '") + _id + "'");
  }

  void commit()
  {
    release(_id, _fd);
  }
};

target_ptr file_target(char const * path)
{
  BOOST_ASSERT(path);
  int const fd( open(path,  O_WRONLY | O_CREAT | O_APPEND | O_SYNC, S_IRUSR | S_IWUSR) );
  if (fd <= 0) throw system_error(string("cannot open '") + path + "' for writing");
  return target_ptr( new fd_target(path, fd) );
}

class child_target : public fd_target
{
  pid_t _child;

public:
  child_target(pid_t child, string const & id, int fd) : fd_target(id, fd), _child(child)
  {
  }

  ~child_target()
  {
    int status;
    waitpid(_child, &status, 0);
    MSG_TRACE("target '" << _id << "' returned code " << status);
  }
};

target_ptr pipe_target(char const * cmd)
{
  BOOST_ASSERT(cmd);

  int   child_fds[2];
  int & read_fd( child_fds[0] );
  int & write_fd( child_fds[1] );

  if (pipe(child_fds) == -1)
    throw system_error("cannot create pipe to sub-process");

  pid_t const child_pid( fork() );
  switch(child_pid)
  {
    case 0:                     // child
      close(write_fd);
      if (dup2(read_fd, STDIN_FILENO) == -1)
      {
        MSG_ERROR("child process '" << cmd << "' cannot read from pipe: " << system_error().what());
        exit(-1);
      }
      close(read_fd);
      execl("/bin/sh", "sh", "-c", cmd, NULL);
      MSG_ERROR("child process cannot execute '" << cmd << "': " << system_error().what());
      exit(-1);

    case -1:                    // Error
      release(cmd, read_fd);
      release(cmd, write_fd);
      throw system_error(string("cannot fork '") + cmd + "'");

    default:                    // child spawned successfully
      release(cmd, read_fd);
  }

  // Switch the pipe into non-blocking mode.

  int rc = fcntl(write_fd, F_GETFL, 0);
  if (rc == -1)
  {
    MSG_ERROR("cannot obtain flags from pipe '" << cmd << "': " << system_error().what());
    goto error_exit;
  }
  rc |= O_NONBLOCK;
  rc = fcntl(write_fd, F_SETFL, rc);
  if (rc == -1)
  {
    MSG_ERROR("cannot set flags for pipe '" << cmd << "': " << system_error().what());
    goto error_exit;
  }

  return target_ptr( new child_target(child_pid, cmd, write_fd) );

error_exit:
  release(cmd, read_fd);
  kill(child_pid, SIGTERM);
  int status;
  waitpid(child_pid, &status, 0);
  return target_ptr();
}
