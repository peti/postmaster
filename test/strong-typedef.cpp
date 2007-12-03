#include <boost/strong_typedef.hpp>
#include <utility>
#include <iostream>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/unit_test.hpp>

typedef std::pair<std::string,std::string> string_pair;

BOOST_STRONG_TYPEDEF(string_pair, address)

inline std::string const & local_part(address const & addr)
{
  return static_cast<string_pair const &>(addr).first;
}

inline std::string const & host_part(address const & addr)
{
  return static_cast<string_pair const &>(addr).second;
}

inline std::ostream & operator<<(std::ostream & os, address const & addr)
{
  return os << local_part(addr) << '@' << host_part(addr);
}

BOOST_AUTO_TEST_CASE( test_strong_typedef )
{
  using namespace std;
  address addr( make_pair("simons", "cryp.to") );
  cout << addr << endl;
}
