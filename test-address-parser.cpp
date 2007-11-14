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
#include <boost/foreach.hpp>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/unit_test.hpp>

struct address_test_case
{
  char const * input;
  char const * user;
  char const * host;
};

BOOST_AUTO_TEST_CASE( test_rfc2821_address_parser )
{
  using namespace std;
  using namespace boost::spirit;
  using namespace phoenix;
  using namespace rfc2822;

  address_test_case const suite[] =
    { { "foo .\tbar @ example\t \t.net ",       "foo.bar",   "example.net" }
    , { " \"\"@example . org",                  "\"\"",      "example.org" }
    , { " \"\\\"\"@[127.0.0.1] ",               "\"\\\"\"",  "[127.0.0.1]" }
    };

  BOOST_FOREACH( address_test_case const & c, suite )
  {
    cout << "parse '" << c.input << "' ... ";
    address addr("<invalid>", "<invalid>");
    parse_info<> const r( parse(c.input, address_p [var(addr) = arg1], wsp_p) );
    cout << addr.first << " @ " << addr.second << std::endl;
    BOOST_CHECK(r.hit);
    BOOST_CHECK_EQUAL(addr.first,  c.user);
    BOOST_CHECK_EQUAL(addr.second, c.host);
  }
}
