/*
 * Copyright (c) 2006-2007 Peter Simons <simons@cryp.to>
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
#include <boost/spirit/fusion/algorithm/push_back.hpp>
#include <vector>

typedef std::vector<address>                    address_list;
typedef std::pair<address, address_list>        route;

bool parse_route(char const * input, route & result)
{
  using namespace spirit;
  using namespace phoenix;
  using namespace boost::fusion;

  route  rt;
  rule<> route_p
    =  (  address_pattern_p                [ var(rt.first) = arg1 ]
       |  ch_p('@')
       )
    >> wsp_p
    >> list_p( *wsp_p >> address_pattern_p [ push_back_a(rt.second) ]
             , ','
             )
    ;
  bool const full_hit( parse(input, route_p).full );
  std::swap(result, rt);
  return full_hit;
}



#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>
#include <boost/foreach.hpp>

struct route_test_case
{
  char const * input;
  char const * lhs_user;
  char const * lhs_host;
  char const * rhs_user;
  char const * rhs_host;
};

BOOST_AUTO_TEST_CASE( test_config_parser )
{
  using namespace std;
  using namespace phoenix;

  route_test_case const suite[] =
    { { "user@domain other.user@other.domain",   "user", "domain", "other.user", "other.domain" }
    , { "user@domain other.user@",               "user", "domain", "other.user", ""             }
    , { "user@domain @domain",                   "user", "domain", "",           "domain"       }
    , { "user@domain user",                      "user", "domain", "user",       ""             }
    , { "user@       other.user@other.domain",   "user", "",       "other.user", "other.domain" }
    , { "user@       other.user@",               "user", "",       "other.user", ""             }
    , { "user@       @domain",                   "user", "",       "",           "domain"       }
    , { "user@       user",                      "user", "",       "user",       ""             }
    , { "user        other.user@other.domain",   "user", "",       "other.user", "other.domain" }
    , { "user        other.user@",               "user", "",       "other.user", ""             }
    , { "user        @domain",                   "user", "",       "",           "domain"       }
    , { "user        user",                      "user", "",       "user",       ""             }
    , { "@domain     other.user@other.domain",   "",     "domain", "other.user", "other.domain" }
    , { "@domain     other.user@",               "",     "domain", "other.user", ""             }
    , { "@domain     @domain",                   "",     "domain", "",           "domain"       }
    , { "@domain     user",                      "",     "domain", "user",       ""             }
    , { "@           other.user@other.domain",   "",     "",       "other.user", "other.domain" }
    , { "@           other.user@",               "",     "",       "other.user", ""             }
    , { "@           @domain",                   "",     "",       "",           "domain"       }
    , { "@           user",                      "",     "",       "user",       ""             }
    };

  BOOST_FOREACH( route_test_case const & c, suite )
  {
    cout << "parse '" << c.input << "' ... ";
    route rt(address("<invalid>", "<invalid>"), address_list());
    bool const full_hit( parse_route(c.input, rt) );
    cout << "'" << rt.first.first << "' @ '" << rt.first.second << "' -> ";
    BOOST_CHECK(full_hit);
    BOOST_REQUIRE_EQUAL(rt.second.size(), 1u);
    cout << "'" << rt.second[0].first << "' @ '" << rt.second[0].second << "'" << std::endl;
    BOOST_CHECK_EQUAL(rt.first.first,   c.lhs_user);
    BOOST_CHECK_EQUAL(rt.first.second,  c.lhs_host);
    BOOST_CHECK_EQUAL(rt.second[0u].first,  c.rhs_user);
    BOOST_CHECK_EQUAL(rt.second[0u].second, c.rhs_host);
  }
}

BOOST_AUTO_TEST_CASE( test_multi_mapping_parser )
{
  using namespace std;
  using namespace phoenix;

  char const * const suite[] =
    { "user@domain other.user@other.domain, claus@ist.der.beste, heinz"
    , "user@       other.user@other.domain, claus@ist.der.beste, heinz"
    , "user        other.user@other.domain, claus@ist.der.beste, heinz"
    , "user        other.user@,             claus@ist.der.beste, heinz"
    , "user        @domain,                 claus@ist.der.beste, heinz"
    , "user        user,                    claus@ist.der.beste, heinz"
    , "@domain     other.user@other.domain, claus@ist.der.beste, heinz"
    , "@domain     other.user@,             claus@ist.der.beste, heinz"
    , "@domain     @domain,                 claus@ist.der.beste, heinz"
    , "@domain     user,                    claus@ist.der.beste, heinz"
    , "@           other.user@other.domain, claus@ist.der.beste, heinz"
    , "@           other.user@,             claus@ist.der.beste, heinz"
    , "@           @domain,                 claus@ist.der.beste, heinz"
    , "@           user,                    claus@ist.der.beste, heinz"
    };

  BOOST_FOREACH( char const * input, suite )
  {
    cout << "parse '" << input << "' ... " << std::endl;
    route rt(address("<invalid>", "<invalid>"), address_list());
    bool const full_hit( parse_route(input, rt) );
    BOOST_CHECK(full_hit);
    BOOST_REQUIRE_EQUAL(rt.second.size(), 3u);
  }
}
