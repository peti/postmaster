#ifndef POSTMASTER_HPP_2007_11_18
#define POSTMASTER_HPP_2007_11_18

#include <vector>
#include <string>
#include <utility>
#include <boost/assert.hpp>
#include <iostream>

#define POSTMASTER_NAME    "postmaster"
#define POSTMASTER_VERSION "2007-05-19"

#ifndef NDEBUG
#  define MSG_TRACE(msg) std::cerr << "trace: [" <<__func__ << "] " << msg << std::endl
#else
#  define MSG_TRACE(msg) ((void)(0))
#endif
#define MSG_INFO(msg)  std::cerr << "info: " << msg << std::endl
#define MSG_ERROR(msg) std::cerr << "*** " << msg << std::endl

using std::string;
typedef std::pair<string,string>        address;
typedef address                         pattern;
typedef pattern                         target;
typedef std::vector<target>             target_list;
typedef std::pair<target, target_list>  route;

bool parse(route &, char const *, char const *);
bool match(address const &, pattern const &);

#endif // POSTMASTER_HPP_2007_11_18
