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
#include <boost/foreach.hpp>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>

typedef std::vector<route> routing_table;

inline bool is_live_target(target const & t)
{
  size_t len( t.first.size() );
  switch (len)
  {
    case 0u: case 1u:   return false;
    default:            return t.first[0] == '<' && t.first[--len] == '>';
  }
}

inline bool is_comment(char const * line)
{
  for(;;)
  {
    switch(*line)
    {
      case '\0': case '#':      return true;
      case ' ': case '\t':      ++line; break;
      default:                  return false;
    }
  }
}

inline std::ostream & operator<< (std::ostream & os, address const & addr)
{
  return os << "'" << addr.first << "' @ '" << addr.second << "'";
}

inline std::ostream & operator<< (std::ostream & os, route const & rt)
{
  os << rt.first << "   --> ";
  BOOST_FOREACH( address const & a, rt.second ) { os << a << " "; }
  return os;
}

inline std::ostream & operator<< (std::ostream & os, routing_table const & rtt)
{
  os << "Routing Table:" << std::endl;
  BOOST_FOREACH( route const & r, rtt ) { os << "  " << r << std::endl; }
  return os;
}

bool route_address(routing_table const & rtt, address const & a, target_list & result)
{
  BOOST_FOREACH( route const & rt, rtt )
  {
    if (match(a, rt.first))
    {
      result.insert(result.end(), rt.second.begin(), rt.second.end());
      return true;
    }
  }
  return false;
}

BOOST_AUTO_TEST_CASE( test_routing )
{
  using namespace std;

  char const * const suite[] =
    { "  #   "
    , "# example config file"
    , "\t\t#\t"
    , "   \t "
    , "postmaster       simons"
    , "simons           file(/tmp/test-mailbox-simons)"
    , "@                pipe(cat >>/tmp/test-mailbox-any)"
    };

  routing_table rtt;

  BOOST_FOREACH( char const * input, suite )
  {
    if (is_comment(input)) continue;
    route rt;
    BOOST_REQUIRE( parse(rt, input, input + strlen(input)) );
    rtt.push_back(rt);
  }

  cout << rtt;
}
