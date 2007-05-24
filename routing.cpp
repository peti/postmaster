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

#include "parser.hpp"

bool parse(route & result, char const * begin, char const * end)
{
  using namespace boost::spirit;
  return parse(begin, end, route_p [assign_a(result)]).full;
}

static bool match(string const & str, string const & patt)
{
  using namespace std;
  return patt.empty() || strcasecmp(str.c_str(), patt.c_str()) == 0;
}

bool match(address const & addr, address const & patt)
{
  return match(addr.first, patt.first) && match(addr.second, patt.second);
}
