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

#include "target.hpp"
#include <boost/compatibility/cpp_c_headers/cstdlib>
#include <boost/foreach.hpp>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>

#define TEST_PATHNAME "/tmp/test-target-tempfile"

static void cleanup()
{
  //remove(TEST_PATHNAME);
}

BOOST_AUTO_TEST_CASE( test_file_target )
{
  using namespace std;

  char const * const body[] =
    { "write a few lines "
    , "and hope they arrive\n"
    , "in the designated file"
    , "\n"
    };

  target_ptr f( file_target(TEST_PATHNAME) );
  atexit(&cleanup);
  BOOST_FOREACH( char const * p, body )
  {
    f->feed(p, p + strlen(p));
  }
  f->commit();
}

BOOST_AUTO_TEST_CASE( test_pipe_target )
{
  using namespace std;

  char const * const body[] =
    { "piping into the same file"
    , "\n"
    };

  target_ptr f( pipe_target("cat >> " TEST_PATHNAME) );
  atexit(&cleanup);
  BOOST_FOREACH( char const * p, body )
  {
    f->feed(p, p + strlen(p));
  }
  f->commit();
}
