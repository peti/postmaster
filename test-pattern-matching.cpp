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

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>

BOOST_AUTO_TEST_CASE( test_config_parser )
{
  // Case doesn't matter in the match.
  BOOST_CHECK(  match(address("claus", "cryp.example.net"), address("CLAUS", "Cryp.example.net")) );
  BOOST_CHECK( !match(address("claus", "cryp.example.net"), address("CLAUS", "example.net")) );

  // An empty user name in the pattern matches any user.
  BOOST_CHECK(  match(address("claus", "example.net"), address("", "example.NET")) );
  BOOST_CHECK(  match(address("heinz", "example.net"), address("", "Example.NET")) );
  BOOST_CHECK( !match(address("claus", "example.net"), address("", "example.org")) );
  BOOST_CHECK( !match(address("heinz", "example.net"), address("", "example.org")) );

  // An empty host name in the pattern matches any host.
  BOOST_CHECK(  match(address("claus", "example.net"), address("claus", "")) );
  BOOST_CHECK( !match(address("heinz", "example.net"), address("claus", "")) );
  BOOST_CHECK(  match(address("claus", "example.org"), address("claus", "")) );
  BOOST_CHECK( !match(address("heinz", "example.org"), address("claus", "")) );
}
