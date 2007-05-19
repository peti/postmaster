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

typedef std::string                     string;
typedef std::pair<string,string>        address;

typedef address                         target;
typedef std::vector<target>             target_list;
typedef std::pair<target, target_list>  route;

extern bool parse_route(char const *, char const *, route &);

#endif // POSTMASTER_HPP_INCLUDED
