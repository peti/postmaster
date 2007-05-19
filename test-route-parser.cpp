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

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>
#include <boost/foreach.hpp>

inline bool parse_route(char const * input, route & result)
{
  using namespace std;
  return parse_route(input, input + strlen(input), result);
}

template <class T, class U>
inline std::ostream & operator<< (std::ostream & os, std::pair<T,U> const & p)
{
  return os << "'" << p.first << "' @ '" << p.second << "'";
}

inline void print_test_result(route const & rt)
{
  using namespace std;
  cout << "result: " << rt.first << "\t|->\t";
  BOOST_FOREACH( address const & a, rt.second ) { cout << a << ", "; }
  cout << endl;
}

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
    cout << "parse '" << c.input << "' ... " << endl;
    route rt(address("@invalid@", "@invalid@"), target_list());
    bool const full_hit( parse_route(c.input, rt) );
    BOOST_CHECK(full_hit);
    print_test_result(rt);
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
    { "user@domain other.user@other.domain,   claus@ist.der.beste, heinz"
    , "user@       other.user@other.domain,   claus@ist.der.beste, heinz"
    , "user        other.user@other.domain,   claus@ist.der.beste, heinz"
    , "user        other.user@,               claus@ist.der.beste, heinz"
    , "user        @domain,                   claus@ist.der.beste, heinz"
    , "user        user,                      claus@ist.der.beste, heinz"
    , "@domain     other.user@other.domain,   claus@ist.der.beste, heinz"
    , "@domain     other.user@,               claus@ist.der.beste, heinz"
    , "@domain     @domain,                   claus@ist.der.beste, heinz"
    , "@domain     user,                      claus@ist.der.beste, heinz"
    , "@           other.user@other.domain,   claus@ist.der.beste, heinz"
    , "@           other.user@,               claus@ist.der.beste, heinz"
    , "@           @domain,                   claus@ist.der.beste, heinz"
    , "@           user,                      claus@ist.der.beste, heinz"
    , "@           pipe(/bin/cat >/dev/null), claus@ist.der.beste, heinz"
    };

  BOOST_FOREACH( char const * input, suite )
  {
    cout << "parse '" << input << "' ... " << endl;
    route rt(address("@invalid@", "@invalid@"), target_list());
    bool const full_hit( parse_route(input, rt) );
    BOOST_CHECK(full_hit);
    print_test_result(rt);
    BOOST_CHECK_EQUAL(rt.second.size(), 3u);
    BOOST_REQUIRE(rt.second.size() >= 3u);
    BOOST_CHECK_EQUAL(rt.second[1u].first,  "claus");
    BOOST_CHECK_EQUAL(rt.second[1u].second, "ist.der.beste");
    BOOST_CHECK_EQUAL(rt.second[2u].first,  "heinz");
    BOOST_CHECK_EQUAL(rt.second[2u].second, "");
  }
}


BOOST_AUTO_TEST_CASE( test_local_mailer_parser )
{
  using namespace std;
  using namespace phoenix;

  char const * const suite[] =
    { "@    @example.org, pipe(/bin/cat >/dev/null), claus"
    , "@    relay([127.0.0.2]), what-ever(/bin/cat >/dev/null), claus"
    };

  BOOST_FOREACH( char const * input, suite )
  {
    cout << "parse '" << input << "' ... " << endl;
    route rt(address("@invalid@", "@invalid@"), target_list());
    bool const full_hit( parse_route(input, rt) );
    BOOST_CHECK(full_hit);
    print_test_result(rt);
    BOOST_REQUIRE_EQUAL(rt.second.size(), 3u);
    BOOST_CHECK_EQUAL(rt.second[2u].first,  "claus");
    BOOST_CHECK_EQUAL(rt.second[2u].second, "");
  }
}
