#ifndef POSTMASTER_ERROR_HPP_2007_11_18
#define POSTMASTER_ERROR_HPP_2007_11_18

/**  \file  error.hpp
     \brief Utilities for reporting system errors.
*/

#include <boost/compatibility/cpp_c_headers/cerrno>
#include <boost/system/system_error.hpp>
#include <boost/bind.hpp>
#include <string>

namespace postmaster
{
  // Error-reporting is courtesy of Boost.System.
  using boost::system::errno_ecat;
  using boost::system::system_error;

  template <class Result, class Cond, class Func>
  Result throw_errno_if(Cond failure, Func f, std::string const & ctx)
  {
    Result r;
    for (r = f(); failure(r); r = f())
    {
      if (errno != EINTR)
      {
        system_error error(errno, errno_ecat, ctx);
        throw error;
      }
    }
    return r;
  }

  template <class Func>
  int throw_errno_if_minus_one(Func f, std::string const & ctx)
  {
    return throw_errno_if<int>(boost::bind(std::equal_to<int>(), -1, _1), f, ctx);
  }
}

#endif // POSTMASTER_ERROR_HPP_2007_11_18
