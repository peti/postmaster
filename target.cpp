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
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
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
  int const fd( open(path,  O_WRONLY | O_CREAT | O_APPEND | O_SYNC, S_IRUSR | S_IWUSR) );
  if (fd <= 0) throw system_error(string("cannot open '") + path + "' for writing");
  return target_ptr( new fd_target(path, fd) );
}


target_ptr pipe_target(char const * cmd)
{
  return target_ptr();
}
