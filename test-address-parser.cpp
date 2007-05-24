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



// BOOST_AUTO_TEST_CASE( test_route_parser_grammar )
// {
//   using namespace std;
//   using namespace boost::spirit;
//   using namespace phoenix;
//
// > How can I append strings to val.second? Accessing the member per
// > se works using a binder:
//
// I'd write a phoenix::function that accesses the /first/ and
// /second/. So, then you can write:
//
//     second(top.val) = construct_<address_list>()
//
// then:
//
//     push_back(second(top.val), some_val)
//
// HTH,
// --
// Joel de Guzman
// http://www.boost-consulting.com
// http://spirit.sf.net
//
//
// -------------------------------------------------------------------------
// This SF.net email is sponsored by DB2 Express
// Download DB2 Express C - the FREE version of DB2 express and take
// control of your XML. No limits. Just data. Click to get it now.
// http://sourceforge.net/powerbar/db2/
//
// }
