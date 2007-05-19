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
  using namespace rfc2822;

  route  rt;

  rule<> route_p
    =  ( address_pattern_p              [ assign_a(rt.first) ]
       | ch_p('@')
       )
    >> wsp_p
    >> list_p
       ( *wsp_p >> ( target_p           [ push_back_a(rt.second) ]
                   | address_pattern_p  [ push_back_a(rt.second) ]
                   )
       , ','
       )
    ;
  bool const full_hit( parse(begin, end, route_p).full );
  std::swap(result, rt);
  return full_hit;
}
