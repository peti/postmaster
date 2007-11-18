#include <boost/noncopyable.hpp>
#include <boost/function/function0.hpp>
#include <boost/function/function1.hpp>
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

    // Popular system error types.

    using boost::system::error_code;
    using boost::system::errno_ecat;
    using boost::system::system_error;

    // Co-operative multi-tasking scheduler.

    class core : private boost::noncopyable
    {
    public:
      typedef boost::function0<void>                    task;

    private:
      typedef std::multimap<time_t,task>                task_queue;
      typedef std::pair<time_t,task_queue::iterator>    task_queue_entry;

    public:
      typedef task_queue_entry                          task_id;

      core() { update(); }

      task_id schedule(task const & t, second_t from_now = 0u)
      {
        BOOST_ASSERT(t);
        if (from_now > 0) update();
        task_queue::iterator const i( _tasks.insert(std::make_pair(_now + from_now, t)) );
        return std::make_pair(i->first, i);
      }

      void cancel(task_id & tid)
      {
        if (tid.first > _now)
        {
          _tasks.erase(tid.second);
          tid.first = 0;
        }
      }

      second_t run()
      {
        while (!_tasks.empty())
        {
          task_queue::iterator const i( _tasks.begin() );
          if (i->first > _now)
          {
            update();
            if (i->first > _now)
              return i->first - _now;
          }
          task t;
          t.swap(i->second);
          _tasks.erase(i);
          t();
        }
        return 0u;
      }

    protected:
      void update() { _now = std::time(0); }

    private:
      time_t            _now;
      task_queue        _tasks;
    };

    // I/O Event Dispatcher.

    class scheduler : public core
    {
    public:
      typedef int socket_id;

      explicit scheduler(unsigned int size_hint = 512u) : _epoll_fd( epoll_create(size_hint) )
      {
        if (_epoll_fd < 0) throw system_error(errno, errno_ecat, "epoll_create(2) failed");
      }

      ~scheduler() { ::close(_epoll_fd); }

      void run()
      {
        for (second_t next_event( core::run() ); !_handlers.empty(); next_event = core::run())
          run_epoll(next_event ? static_cast<int>(next_event) * 1000 : -1);
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
          throw system_error(errno, errno_ecat, "scheduler::register_socket() failed");
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
          throw system_error(errno, errno_ecat, "scheduler::unregister_socket() failed");
      }

      void on_input(socket_id s, task t)
      {
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) != _handlers.end());
        handler & h( _handlers[s] );
        h.first.swap(t);
        if ( static_cast<bool>(t) != static_cast<bool>(h.first) )
          modify_epoll(s, h, "scheduler::on_input() failed");
      }

      void on_output(socket_id s, task t)
      {
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) != _handlers.end());
        handler & h( _handlers[s] );
        h.second.swap(t);
        if ( static_cast<bool>(t) != static_cast<bool>(h.first) )
          modify_epoll(s, h, "scheduler::on_output() failed");
      }

    private:
      typedef std::pair<task,task>        handler;
      typedef std::map<socket_id,handler> handler_map;

      socket_id         _epoll_fd;
      handler_map       _handlers;

      void run_epoll(int timeout)
      {
        epoll_event ev[32u];
        int const rc( epoll_wait(_epoll_fd, ev, sizeof(ev) / sizeof(epoll_event), timeout) );
        if (rc < 0) throw system_error(errno, errno_ecat, "scheduler::run_epoll() failed");
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

      void modify_epoll(socket_id s, handler & h, char const * ctxid)
      {
        epoll_event ev;
        ev.data.fd = s;
        ev.events  = (h.first ? EPOLLIN : 0) | (h.second ? EPOLLOUT : 0);
        if (epoll_ctl(_epoll_fd, EPOLL_CTL_MOD, s, &ev) != 0)
          throw system_error(errno, errno_ecat, ctxid);
      }
    };

    // Basic I/O interface.

    class system_socket;
    typedef boost::shared_ptr<system_socket> socket;

    class system_socket : private boost::noncopyable
    {
    public:
      typedef boost::function1<void, socket> handler;

      ~system_socket()
      {
        _io.unregister_socket(_sock);
        if (_close_on_destruction) ::close(_sock);
      }

      void close_on_destruction(bool b) { _close_on_destruction = b; }

      friend socket create_socket(scheduler & io, int fd)
      {
        socket s;
        s.reset( new system_socket(io, fd) );
        return s;
      }

      friend void on_input(socket s, handler f, second_t timeout = 0, handler h = handler())
      {
        s->_input.set<&scheduler::on_input>(s, f, timeout, h);
      }

      friend void on_output(socket s, handler f, second_t timeout = 0, handler h = handler())
      {
        s->_output.set<&scheduler::on_output>(s, f, timeout, h);
      }

    private:
      system_socket(scheduler & io, int fd) : _io(io), _sock(fd), _close_on_destruction(true)
      {
        BOOST_ASSERT(fd >= 0);
        _io.register_socket(_sock);
      }

      scheduler &               _io;
      int const                 _sock;
      bool                      _close_on_destruction;

      typedef void (scheduler::*registrar)(scheduler::socket_id, scheduler::task);

      class context : private boost::noncopyable
      {
      public:
        template <registrar on_event>
        void set(socket s, handler f, second_t to, handler h)
        {
          system_socket & sock( *s );
          sock._io.cancel(_timeout_id);
          _f.swap(f);
          _h.swap(h);
          _timeout = to;
          if (_f)
          {
            if (_timeout)
              _timeout_id = sock._io.schedule(boost::bind(&context::handle_timeout<on_event>, this, s), _timeout);
            (sock._io.*on_event)(sock._sock, boost::bind(&context::handle_event<on_event>, this, s));
          }
          else
            (sock._io.*on_event)(sock._sock, scheduler::task());
        }

      private:
        handler                 _f;
        handler                 _h;
        scheduler::task_id      _timeout_id;
        second_t                _timeout;

        template <registrar on_event>
        void handle_event(socket s)
        {
          system_socket & sock( *s );
          sock._io.cancel(_timeout_id);
          if (_f) _f(s);
          if (_f)
          {
            if (_timeout)
              _timeout_id = sock._io.schedule(boost::bind(&context::handle_timeout<on_event>, this, s), _timeout);
          }
          else
            (sock._io.*on_event)(sock._sock, scheduler::task());
        }

        template <registrar on_event>
        void handle_timeout(socket s)
        {
          handler h;
          h.swap(_h);
          set<on_event>(s, handler(), 0, handler());
          if (h) h(s);
        }
      };

      context _input, _output;
    };
  }
}

void print_id(unsigned int id) { std::cout << "task id " << id << std::endl; }

void socket_timeout(postmaster::io::socket s1, postmaster::io::socket s2, postmaster::io::socket s)
{
  BOOST_ASSERT(s == s1 || s == s2);
  std::cout << "timeout on socket " << s << std::endl;
  postmaster::io::on_input(s1,  postmaster::io::system_socket::handler());
  postmaster::io::on_output(s1, postmaster::io::system_socket::handler());
  postmaster::io::on_input(s2,  postmaster::io::system_socket::handler());
  postmaster::io::on_output(s2, postmaster::io::system_socket::handler());
}

int main(int, char**)
{
  using namespace std;
  using namespace postmaster::io;
  using boost::bind;

  static scheduler io;
  {
    socket sin( create_socket(io, STDIN_FILENO) ); sin->close_on_destruction(false);
    socket sout( create_socket(io, STDOUT_FILENO) ); sout->close_on_destruction(false);
    cout << "standard input  = socket " << sin << endl
         << "standard output = socket " << sout << endl;
    on_input(sin, bind(print_id, 10u), 3u, bind(socket_timeout, sin, sout, _1));
    on_output(sout, bind(print_id, 20u), 5u, bind(socket_timeout, sin, sout, _1));
  }

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
