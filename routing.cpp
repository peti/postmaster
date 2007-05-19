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
#include "rfc2822/skipper.hpp"
#include <boost/spirit/phoenix/binders.hpp>
#include <boost/foreach.hpp>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>

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

struct test_case
{
  char const * input;
  char const * user;
  char const * host;
};

BOOST_AUTO_TEST_CASE( test_rfc2821_address_parser )
{
  using namespace std;
  using namespace phoenix;

  test_case const suite[] =
    { { "foo .\tbar @ example\t \t.net ",       "foo.bar",   "example.net" }
    , { "\"\"@example . org",                   "\"\"",      "example.org" }
    , { "\"\\\"\"@[127.0.0.1]",                 "\"\\\"\"",  "[127.0.0.1]" }
    };

  BOOST_FOREACH( test_case const & c, suite )
  {
    cout << "parse '" << c.input << "' ... ";
    address addr;
    spirit::parse_info<> const r( parse(c.input, address_p [var(addr) = arg1], wsp_p) );
    BOOST_REQUIRE(r.hit);
    BOOST_REQUIRE_EQUAL(addr.first,  c.user);
    BOOST_REQUIRE_EQUAL(addr.second, c.host);
    cout << addr.first << " @ " << addr.second << std::endl;
  }
}

typedef std::pair<address,address> route;

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
    spirit::rule<scannerT>    top;
    spirit::subrule<0>        stmt;
    spirit::subrule<1>        lhs;
    spirit::subrule<2>        rhs;

    definition(route_parser const & self)
    {
      using namespace spirit;
      using namespace phoenix;

      top =
        ( stmt =  lhs >> +wsp_p >> rhs >> !cr_p >> lf_p

        , lhs  =  (  address_p             [bind(&route::first)(self.val) = arg1]
                  |  local_part_p          [bind(&address::first)(bind(&route::first)(self.val)) = arg1] >> "@"
                  |  ch_p('@') >> domain_p [bind(&address::second)(bind(&route::first)(self.val)) = arg1]
                  )

        , rhs  =  (  address_p             [bind(&route::second)(self.val) = arg1]
                  |  local_part_p          [bind(&address::first)(bind(&route::second)(self.val)) = arg1] >> "@"
                  |  ch_p('@') >> domain_p [bind(&address::second)(bind(&route::second)(self.val)) = arg1]
                  )
        );
    }

    spirit::rule<scannerT> const & start() const { return top; }
  };
};

route_parser const  route_p;

BOOST_AUTO_TEST_CASE( test_config_parser )
{
  using namespace std;
  using namespace phoenix;

  char const * const suite[] =
    { "user@domain     other.user@other.domain\r\n"
    , "user@domain     other.user@\r\n"
    , "user@domain     @domain\r\n"
    , "user@           alias.for.user@any.other.domain\n"
    , "user@           other.user@\r\n"
    , "user@           @domain\r\n"
    , "@domain         alias.for.user@any.other.domain\n"
    , "@domain         other.user@\r\n"
    , "@domain         @domain\r\n"
    };

  BOOST_FOREACH( char const * input, suite )
  {
    cout << "route '" << input << "' ... ";
    spirit::parse_info<> const r( parse(input, route_p) );
    BOOST_REQUIRE(r.hit);
    BOOST_REQUIRE(r.full);
    cout << std::endl;
  }
}
