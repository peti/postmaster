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

#ifndef POSTMASTER_TARGET_HPP_INCLUDED
#define POSTMASTER_TARGET_HPP_INCLUDED

#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>

struct live_target : private boost::noncopyable
{
  virtual ~live_target() = 0;
  virtual void feed(char const *, char const *) = 0;
  virtual void commit() = 0;
};

typedef boost::shared_ptr<live_target> target_ptr;

target_ptr file_target(char const * path);
target_ptr pipe_target(char const * cmd);

#endif // POSTMASTER_TARGET_HPP_INCLUDED
