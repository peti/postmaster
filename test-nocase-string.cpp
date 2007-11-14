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

#include "nocase-string.hpp"

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE( test_nocase_string )
{
  typedef nocase_string<char> casestr;

  BOOST_CHECK_EQUAL( casestr("test"), casestr("TEST") );
  BOOST_CHECK_EQUAL( casestr("TEST"), casestr("test") );

  BOOST_CHECK( !( casestr("test") < casestr("TEST")) );
  BOOST_CHECK( !( casestr("test") > casestr("TEST")) );
  BOOST_CHECK( !( casestr("TEST") < casestr("test")) );
  BOOST_CHECK( !( casestr("TEST") > casestr("test")) );
}
