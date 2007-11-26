#ifndef POSTMASTER_ERROR_HPP_2007_11_18
#define POSTMASTER_ERROR_HPP_2007_11_18

/**  \file  error.hpp
     \brief Utilities for reporting system errors.
*/

#include <boost/compatibility/cpp_c_headers/cerrno>
#include <boost/bind.hpp>
#include <string>
#include <stdexcept>
#include <boost/assert.hpp>

#ifndef NDEBUG
#  include <iostream>
#  define TRACE_MSG(msg)        std::cerr << "[" << __func__ << "] " << msg << std::endl
#  define TRACE_ARG1(arg)              TRACE_MSG(#arg " = " << arg)
#  define TRACE_ARG2(arg1,arg2)        TRACE_MSG(#arg1 " = " << arg1 << ", " << #arg2 " = " << arg2)
#else
#  define TRACE_MSG(msg)        ((void)(0))
#  define TRACE_ARG1(arg)       ((void)(0))
#  define TRACE_ARG2(arg1,arg2) ((void)(0))
#endif

namespace postmaster
{
  struct system_error : public std::runtime_error
  {
    explicit system_error(int errc) : std::runtime_error(std::strerror(errc)) { }
    system_error(int errc, std::string const & msg) : std::runtime_error(msg + ": " + std::strerror(errc)) { }
  };

  template <class Result, class Cond, class Func>
  Result throw_errno_if(Cond failure, Func f, std::string const & ctx)
  {
    Result r;
    for (r = f(); failure(r); r = f())
    {
      if (errno != EINTR)
      {
        system_error error(errno, ctx);
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
