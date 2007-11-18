#include <boost/noncopyable.hpp>
#include <boost/function/function0.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/system/system_error.hpp>
#include <boost/compatibility/cpp_c_headers/cstddef>
#include <boost/compatibility/cpp_c_headers/ctime>
#include <boost/compatibility/cpp_c_headers/cerrno>
#include <boost/bind.hpp>
#include <algorithm>
#include <iostream>
#include <map>
#include <sys/uio.h>
#include <sys/epoll.h>

#define TRACE(msg) std::cout << __PRETTY_FUNCTION__ << msg << std::endl

namespace postmaster
{
  namespace io
  {
    // Basic system types for this platform.

    using std::size_t;
    using std::ptrdiff_t;
    using std::time_t;
    typedef unsigned int second_t;

    // Errors

    using boost::system::error_code;
    using boost::system::errno_ecat;
    using boost::system::system_error;

    // Co-operative multi-tasking scheduler.

    class scheduler : private boost::noncopyable
    {
    public:
      typedef boost::function0<void>                    task;

    private:
      typedef std::multimap<time_t,task>                task_queue;
      typedef std::pair<time_t,task_queue::iterator>    task_queue_entry;

    public:
      typedef task_queue_entry                          task_id;
      typedef int                                       socket_id;

      explicit scheduler(int size_hint = 512u) : _epoll_fd( epoll_create(size_hint) )
      {
        if (_epoll_fd < 0) throw system_error(errno, errno_ecat, "epoll_create(2) failed");
        update();
      }

      ~scheduler() { ::close(_epoll_fd); }

      task_id schedule(task const & t, second_t delay = 0u)
      {
        BOOST_ASSERT(t);
        if (delay > 0) update();
        task_queue::iterator const i( _tasks.insert(std::make_pair(_now + delay, t)) );
        return std::make_pair(i->first, i);
      }

      void cancel(task_id const & tid)
      {
        if (tid.first > _now) _tasks.erase(tid.second);
      }

      void run()
      {
        for (;;)
        {
          if (_tasks.empty())
          {
            if (_handlers.empty()) break;
            run_epoll(-1);
            continue;
          }
          else
          {
            task_queue::iterator const i( _tasks.begin() );
            if (i->first > _now)
            {
              update();
              if (i->first > _now)
              {
                run_epoll(static_cast<int>(i->first - _now) * 1000);
                continue;
              }
            }
            task t;
            t.swap(i->second);
            _tasks.erase(i);
            t();
          }
        }
      }

      void register_socket(socket_id const & s)
      {
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) == _handlers.end());
        _handlers[s] = handler();
        epoll_event ev;
        ev.data.fd = s;
        ev.events  = 0;
        if (epoll_ctl(_epoll_fd, EPOLL_CTL_ADD, s, &ev) != 0)
        {
          _handlers.erase(s);
          throw system_error(errno, errno_ecat, "register_socket: epoll_ctl(2) failed");
        }
      }

      void unregister_socket(socket_id const & s)
      {
        BOOST_ASSERT(s >= 0);
        handler_map::iterator const i( _handlers.find(s) );
        BOOST_ASSERT(i != _handlers.end());
        _handlers.erase(i);
        epoll_event ev;
        ev.data.fd = s;
        ev.events  = 0;
        if (epoll_ctl(_epoll_fd, EPOLL_CTL_DEL, s, &ev) != 0)
          throw system_error(errno, errno_ecat, "unregister_socket: epoll_ctl(2) failed");
      }

      void on_input(socket_id s, task t)
      {
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) != _handlers.end());
        handler & h( _handlers[s] );
        h.first.swap(t);
        if ( (t && h.first) || (!t && !h.first) ) return;
        epoll_event ev;
        ev.data.fd = s;
        ev.events  = (h.first ? EPOLLIN : 0) | (h.second ? EPOLLOUT : 0);
        if (epoll_ctl(_epoll_fd, EPOLL_CTL_MOD, s, &ev) != 0)
          throw system_error(errno, errno_ecat, "on_input: epoll_ctl(2) failed");
      }

      void on_output(socket_id s, task t)
      {
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) != _handlers.end());
        handler & h( _handlers[s] );
        h.second.swap(t);
        if ( (t && h.second) || (!t && !h.second) ) return;
        epoll_event ev;
        ev.data.fd = s;
        ev.events  = (h.first ? EPOLLIN : 0) | (h.second ? EPOLLOUT : 0);
        if (epoll_ctl(_epoll_fd, EPOLL_CTL_MOD, s, &ev) != 0)
          throw system_error(errno, errno_ecat, "on_output: epoll_ctl(2) failed");
      }

    private:
      void run_epoll(int timeout)
      {
        epoll_event ev[32u];
        int const rc( epoll_wait(_epoll_fd, ev, sizeof(ev) / sizeof(epoll_event), timeout) );
        if (rc < 0) throw system_error(errno, errno_ecat, "run: epoll_wait(2) failed");
        std::cout << rc << " sockets active" << std::endl;
        update();
        for (int i(0); i != rc; ++i)
        {
          BOOST_ASSERT(ev[i].data.fd >= 0);
          BOOST_ASSERT(_handlers.find(ev[i].data.fd) != _handlers.end());
          handler & h( _handlers[ev[i].data.fd] );
          if ((ev[i].events & EPOLLIN)  && h.first)  schedule(h.first);
          if ((ev[i].events & EPOLLOUT) && h.second) schedule(h.second);
        }
      }

      void update() { _now = std::time(0); }

      time_t            _now;
      task_queue        _tasks;

      typedef std::pair<task,task>        handler;
      typedef std::map<socket_id,handler> handler_map;

      socket_id         _epoll_fd;
      handler_map       _handlers;
    };

    // Basic I/O interface.

    class system_socket : private boost::noncopyable
    {
    public:
      explicit system_socket(scheduler & io, int fd) : _io(io), _sock(fd), _close_on_destruction(true)
      {
        BOOST_ASSERT(fd >= 0);
      }

      ~system_socket()
      {
        if (_close_on_destruction) ::close(_sock);
      }

      size_t read(error_code &, void *, size_t);
      size_t write(error_code &, void const *, size_t);

      size_t readv(error_code &, iovec const *, size_t);
      size_t writev(error_code &, iovec const *, size_t);

    protected:
      scheduler &       _io;
      int const         _sock;
      bool              _close_on_destruction;
    };

    struct weak_socket : public system_socket
    {
      explicit weak_socket(scheduler & io, int fd) : system_socket(io, fd) { _close_on_destruction = false; }
    };
  }
}

void print_id(unsigned int id) { std::cout << "task id " << id << std::endl; }

int main(int, char**)
{
  using namespace std;
  using namespace postmaster::io;
  using boost::bind;

  scheduler io;

  io.register_socket(STDIN_FILENO);
  io.schedule(bind(&scheduler::unregister_socket, &io, STDIN_FILENO), 5u);
  io.on_input(STDIN_FILENO, bind(print_id, 10u));

  io.register_socket(STDOUT_FILENO);
  io.schedule(bind(&scheduler::unregister_socket, &io, STDOUT_FILENO), 2u);
  io.on_output(STDOUT_FILENO, bind(print_id, 20u));


  io.schedule(bind(print_id, 3u), 1u);
  io.schedule(bind(print_id, 0u));
  io.schedule(bind(print_id, 4u), 3u);
  io.schedule(bind(print_id, 1u));
  io.schedule(bind(print_id, 2u));

  int pipe_fds[2];
  if (pipe(pipe_fds) != 0)
    throw system_error(errno, errno_ecat, "pipe(2) failed");

  for(;;)
  {
    try
    {
      io.run();
      break;
    }
    catch(exception const & e)
    {
      std::cout << "*** I/O main loop caught: " << e.what() << endl;
    }
  }

  cout << "postmaster shut down" << endl;
  return 0;
}
