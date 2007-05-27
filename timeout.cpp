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
#include <map>
#include <algorithm>
#include <boost/compatibility/cpp_c_headers/ctime>
#include <boost/function.hpp>
#include <boost/test/included/prg_exec_monitor.hpp>
#include <boost/bind.hpp>
#include <boost/noncopyable.hpp>

using std::time_t;

class system_time : private boost::noncopyable
{
  static time_t _now;

public:
  system_time() { update(); }

  static time_t const & now() { return _now; }

  static void update()
  {
    using namespace std;
    time_t new_now;
    if (time(&new_now) == time_t(-1))
      throw system_error("cannot determine system time");
    else
      _now = new_now;
  }
};

time_t system_time::_now = time_t(0);

class timeout : public system_time
{
public:
  typedef boost::function<void ()>      handler;
  typedef std::multimap<time_t,handler> handler_map;
  typedef handler_map::iterator         iterator;
  typedef handler_map::value_type       context;
  typedef std::pair<time_t,iterator>    event;

  event at(time_t const & to, handler const & f)
  {
    BOOST_ASSERT(f);
    BOOST_ASSERT(to >= now());
    return event(to, _the_map.insert(context(to, f)));
  }

  event in(unsigned int seconds, handler const & f)
  {
    return at(now() + seconds, f);
  }

  void cancel(event const & to)
  {
    if (to.first >= now()) _the_map.erase(to.second);
  }

  bool empty()
  {
    return _the_map.empty();
  }

  size_t deliver(unsigned int * next_in_seconds = 0)
  {
    update();
    size_t n( 0u );
    while (!empty())
    {
      iterator i( _the_map.begin() );
      if (i->first < now())
      {
        ++n;
        i->second();
        _the_map.erase(i);
      }
      else
      {
        if (next_in_seconds)
          *next_in_seconds = static_cast<unsigned int>(i->first - now()) + 1u;
        break;
      }
    }
    return n;
  }

private:
  handler_map _the_map;
};

template <unsigned int N>
static void callback()
{
  MSG_TRACE(N << " at " << system_time::now());
}

int cpp_main(int, char ** argv)
{
  using namespace std;
  using namespace boost;

  timeout to;
  timeout::event const ev2( to.in(2u, &callback<2u>) );
  timeout::event const ev5( to.in(5u, &callback<5u>) );
  to.in(3u, bind(&timeout::cancel, &to, ev2));
  to.in(3u, bind(&timeout::cancel, &to, ev5));
  to.in(4u, bind(&timeout::in, &to, 2, &callback<6u>));

  for (unsigned int idle_time; !to.empty(); sleep(idle_time))
  {
    size_t const n( to.deliver(&idle_time) );
    MSG_TRACE("delivered " << n << " timeouts");
  }

  return 0;
}
