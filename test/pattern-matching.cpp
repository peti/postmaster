#include "postmaster.hpp"

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE( test_pattern_matching )
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
