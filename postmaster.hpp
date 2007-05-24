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

#ifndef POSTMASTER_HPP_INCLUDED
#define POSTMASTER_HPP_INCLUDED

#include <vector>
#include <string>
#include <utility>
#include <boost/assert.hpp>
#include <boost/system/system_error.hpp>
#include <iostream>

#define POSTMASTER_NAME    "postmaster"
#define POSTMASTER_VERSION "2007-05-19"

#ifndef NDEBUG
#  define MSG_TRACE(msg) std::cerr << "trace: [" <<__func__ << "] " << msg << std::endl
#else
#  define MSG_TRACE(msg) ((void)(0))
#endif
#define MSG_INFO(msg)  std::cerr << "info: " << msg << std::endl
#define MSG_ERROR(msg) std::cerr << "*** " << msg << std::endl

using std::string;
typedef std::pair<string,string>        address;
typedef address                         pattern;
typedef pattern                         target;
typedef std::vector<target>             target_list;
typedef std::pair<target, target_list>  route;

bool parse(route &, char const *, char const *);
bool match(address const &, pattern const &);

struct system_error : public boost::system::system_error
{
  system_error();
  explicit system_error(string const & context);
};

#endif // POSTMASTER_HPP_INCLUDED
