#ifndef POSTMASTER_TARGET_HPP_2007_11_18
#define POSTMASTER_TARGET_HPP_2007_11_18

#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>

struct live_target : private boost::noncopyable
{
  virtual ~live_target() = 0;
  virtual void feed(char const *, char const *) = 0;
  virtual void commit() = 0;
};

typedef boost::shared_ptr<live_target> target_ptr;

target_ptr file_target(char const * path);
target_ptr pipe_target(char const * cmd);

#endif // POSTMASTER_TARGET_HPP_2007_11_18
