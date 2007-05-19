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

#ifndef POSTMASTER_NOCASE_STRING_HPP_INCLUDED
#define POSTMASTER_NOCASE_STRING_HPP_INCLUDED

#include <string>
#include <iosfwd>
#include <boost/compatibility/cpp_c_headers/cstring>

template <class T>
struct case_char_traits : public std::char_traits<T>
{
  typedef T                             char_type;
  typedef std::char_traits<char_type>   base;
  typedef typename base::int_type       int_type;
  typedef typename base::pos_type       pos_type;
  typedef typename base::off_type       off_type;
  typedef typename base::state_type     state_type;

  static bool eq(char_type const & c1, char_type const & c2)
  {
    using namespace std;
    return tolower(c1) == tolower(c2);
  }

  static bool lt(char_type const & c1, char_type const & c2)
  {
    using namespace std;
    return tolower(c1) < tolower(c2);
  }

  static int compare(char_type const * s1, char_type const * s2, std::size_t n)
  {
    using namespace std;
    return strncasecmp(s1, s2, n);
  }

  static char_type const * find(char_type const * s, size_t n, char_type const & a)
  {
    for (/**/; n; ++s, --n)
      if (eq(*s, a))
        return s;
    return 0;
  }
};

template < class T = char, class Allocator = std::allocator<T> >
struct nocase_string : public std::basic_string<T, case_char_traits<T>, Allocator>
{
  typedef std::basic_string<T, case_char_traits<T>, Allocator>  base;
  typedef std::basic_string<T, std::char_traits<T>, Allocator>  case_string;

  nocase_string() { }

  template <class Iterator>
  nocase_string(Iterator b, Iterator e)           : base(b, e) { }

  explicit nocase_string(case_string const & str) : base(str.begin(), str.end()) { }
  explicit nocase_string(char const * p)          : base(p) { }
};

template <class T, class Alloc>
inline std::ostream & operator<< (std::ostream & os, nocase_string<T,Alloc> const & str)
{
  return os << str.c_str();
}

#endif // POSTMASTER_NOCASE_STRING_HPP_INCLUDED
