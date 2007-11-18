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

      void cancel(task_id const & tid)
      {
        if (tid.first > _now) _tasks.erase(tid.second);
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

      typedef std::pair<task,task>        handler;
      typedef std::map<socket_id,handler> handler_map;

      socket_id         _epoll_fd;
      handler_map       _handlers;
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

      char * recv(char *, char *);
      char * send(char *, char *);
      char const * send(char const *, char const *);

      friend socket create_socket(scheduler & io, int fd)
      {
        socket s;
        s.reset( new system_socket(io, fd) );
        return s;
      }

      friend void on_input(socket s, handler f, second_t timeout = 0, handler h = handler())
      {
        s->_input.set(s, f, timeout, h, &scheduler::on_input);
      }

      friend void on_output(socket s, handler f, second_t timeout = 0, handler h = handler())
      {
        s->_output.set(s, f, timeout, h, &scheduler::on_output);
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

      struct context
      {
        handler                   f;
        handler                   h;
        scheduler::task_id        timeout_id;
        second_t                  timeout;

        void set(socket s, handler f_, second_t to, handler h_, registrar on_event)
        {
          system_socket & self( *s );
          self._io.cancel(timeout_id);
          timeout_id = scheduler::task_id();
          f.swap(f_);
          h.swap(h_);
          timeout = to;
          if (f)
          {
            if (h && timeout)
              timeout_id = self._io.schedule(boost::bind(handle_timeout, s, on_event), timeout);
            (self._io.*on_event)(self._sock, boost::bind(handle_event, s, on_event));
          }
          else
            (self._io.*on_event)(self._sock, scheduler::task());
        }
      };

      context _input, _output;

      static void handle_event(socket s, registrar on_event)
      {
        system_socket & self( *s );
        context & ctx( on_event == &scheduler::on_input ? self._input : self._output );
        self._io.cancel(ctx.timeout_id);
        if (ctx.f) ctx.f(s);
        if (ctx.f)
        {
          if (ctx.h && ctx.timeout)
            ctx.timeout_id = self._io.schedule(boost::bind(handle_timeout, s, on_event), ctx.timeout);
        }
        else
          (self._io.*on_event)(self._sock, scheduler::task());
      }

      static void handle_timeout(socket s, registrar on_event)
      {
        system_socket & self( *s );
        context & ctx( on_event == &scheduler::on_input ? self._input : self._output );
        handler f, h;
        f.swap(ctx.f);
        h.swap(ctx.h);
        (self._io.*on_event)(self._sock, scheduler::task());
        if (h) h(s);
      }
    };

    class echo_handler
    {
    public:
    };
  }
}

void print_id(unsigned int id) { std::cout << "task id " << id << std::endl; }

void socket_timeout(postmaster::io::socket s)
{
  std::cout << "timeout on socket " << s << std::endl;
}

int main(int, char**)
{
  using namespace std;
  using namespace postmaster::io;
  using boost::bind;

  static scheduler io;

  socket s( create_socket(io, STDIN_FILENO) ); s->close_on_destruction(false);
  on_input(s, bind(print_id, 10u), 5u, bind(socket_timeout, s));
  s.reset();


//   socket sout( create_socket(io, STDOUT_FILENO) ); sout->close_on_destruction(false);
//   socket serr( create_socket(io, STDERR_FILENO) ); serr->close_on_destruction(false);

//   io.schedule(bind(&boost::shared_ptr<system_socket>::reset, boost::ref(sin)), 5u);
//   io.on_input(STDIN_FILENO, bind(print_id, 10u));
//
//   io.schedule(bind(&scheduler::unregister_socket, &io, STDOUT_FILENO), 2u);
//   io.on_output(STDOUT_FILENO, bind(print_id, 20u));
//

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
