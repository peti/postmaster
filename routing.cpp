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

#if 0                           // enable spirit debugging
#  define BOOST_SPIRIT_DEBUG
#  define BOOST_SPIRIT_DEBUG_OUT std::cerr
#endif

#include "rfc2822/address.hpp"
#include <boost/spirit/phoenix/binders.hpp>
#include <vector>
#include <boost/assert.hpp>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>
#include <boost/foreach.hpp>

using namespace rfc2822;

typedef std::pair<std::string,std::string> address;

struct address_closure : public spirit::closure<address_closure, address>
{
  member1 val;
};

struct address_parser : public spirit::grammar<address_parser, address_closure::context_t>
{
  address_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT>    top;

    definition(address_parser const & self)
    {
      using namespace spirit;
      using namespace phoenix;

      top
        = local_part_p [bind(&address::first)(self.val)  = arg1]
          >> ch_p('@')
          >> domain_p  [bind(&address::second)(self.val) = arg1]
        ;
    }

    spirit::rule<scannerT> const & start() const { return top; }
  };
};

address_parser const  address_p;

struct address_test_case
{
  char const * input;
  char const * user;
  char const * host;
};

BOOST_AUTO_TEST_CASE( test_rfc2821_address_parser )
{
  using namespace std;
  using namespace phoenix;

  address_test_case const suite[] =
    { { "foo .\tbar @ example\t \t.net ",       "foo.bar",   "example.net" }
    , { " \"\"@example . org",                  "\"\"",      "example.org" }
    , { " \"\\\"\"@[127.0.0.1] ",               "\"\\\"\"",  "[127.0.0.1]" }
    };

  BOOST_FOREACH( address_test_case const & c, suite )
  {
    cout << "parse '" << c.input << "' ... ";
    address addr("<invalid>", "<invalid>");
    spirit::parse_info<> const r( parse(c.input, address_p [var(addr) = arg1], wsp_p) );
    cout << addr.first << " @ " << addr.second << std::endl;
    BOOST_CHECK(r.hit);
    BOOST_CHECK_EQUAL(addr.first,  c.user);
    BOOST_CHECK_EQUAL(addr.second, c.host);
  }
}

typedef std::vector<address>                    address_list;
typedef std::pair<address, address_list>        route;

struct route_closure : public spirit::closure<route_closure, route>
{
  member1 val;
};

struct route_parser : public spirit::grammar<route_parser, route_closure::context_t>
{
  route_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT, route_closure::context_t>     top;
    spirit::rule<scannerT, address_closure::context_t>   lhs;
    spirit::rule<scannerT, address_closure::context_t>   rhs;
    spirit::rule<scannerT, address_closure::context_t>   addr;

    definition(route_parser const & self)
    {
      using namespace spirit;
      using namespace phoenix;

      top =  lhs                        [ bind(&route::first)(top.val)  = arg1]
             >> wsp_p
             >> list_p
             (  *wsp_p >> rhs
             ,  ','
             );

      lhs =  addr [lhs.val = arg1] | ch_p('@');

      rhs =  addr [rhs.val = arg1];

      addr = (  ch_p('@') >> domain_p   [bind(&address::second)(addr.val) = arg1]
             |  address_p               [addr.val = arg1]
             |  local_part_p            [bind(&address::first)(addr.val) = arg1] >> !ch_p('@')
             );
    }

    spirit::rule<scannerT, route_closure::context_t> const & start() const { return top; }
  };
};

route_parser const  route_p;

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
    spirit::parse_info<> const r( parse(c.input, route_p [var(rt) = arg1]) );
    cout << "'" << rt.first.first << "' @ '" << rt.first.second << "' -> ";
    BOOST_CHECK(r.hit);
    BOOST_CHECK(r.full);
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
    cout << "parse '" << input << "' ... ";
    route rt(address("<invalid>", "<invalid>"), address_list());
    spirit::parse_info<> const r( parse(input, route_p [var(rt) = arg1]) );
    cout << "rest: '" << r.stop << "'" << endl;;
    BOOST_CHECK(r.hit);
    BOOST_CHECK(r.full);
  }
}
