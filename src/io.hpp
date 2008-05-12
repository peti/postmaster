#ifndef POSTMASTER_IO_HPP_2007_11_18
#define POSTMASTER_IO_HPP_2007_11_18

/** \file  io.hpp
 *  \brief Asynchronous I/O Core
 *
 * The I/O core is basically a register of callback functions, each of them
 * attached to one of the following events:
 *
 *  - a socket becomes readable or writable,
 *  - a child process terminates (and produces an exit code),
 *  - a DNS response has been received, or
 *  - a pre-set timeout expires.
 *
 * The main event loop is structured as follows:
 *
 *  1. Block all signals.
 *  2. Collect exit codes from child processes; invoke the attached callbacks.
 *  3. Invoke all scheduled ready tasks.
 *  4. Are there still sockets or timeouts registered? No: shut down.
 *  5. Enable signals.
 *  6. Block with epoll_wait() until there is socket I/O, a signal, or timeout.
 *  7. Did we have socket activity? Yes: schedule the attached callbacks.
 *  8. Repeat from 1.
 *
 * With the exception of poll_wait(), all other system calls are performed
 * while signals are blocked, so there should be no interruptions. If EINTR
 * does occur for whatever reason, the implementation ignores it and re-tries
 * the system call. EINTR never causes an exception.
 *
 * The implementation is re-entrant.
 */

#include "error.hpp"
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/compatibility/cpp_c_headers/cstddef>
#include <boost/compatibility/cpp_c_headers/ctime>
#include <boost/bind.hpp>
#include <map>
#include <functional>
#include <vector>
#include <set>
#include <adns.h>
#include <poll.h>
#include <iostream>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <sys/wait.h>

namespace postmaster
{
  // Basic system types for this platform.

  using std::size_t;
  using std::ptrdiff_t;
  using std::time_t;
  typedef unsigned int second_t;

  namespace io
  {
    // Child process management.

    class system : private boost::noncopyable
    {
    public:
      typedef int                                 exit_code;
      typedef boost::function<void (exit_code)>   exit_handler;
      typedef pid_t                               child_id;

      system()
      {
        struct sigaction a;     // disable all signals but SIGCHLD
        std::memset(&a, 0, sizeof(a));
        a.sa_handler = SIG_IGN;
        for (int i(0); i != 32; ++i) sigaction(i, &a, 0);
        a.sa_handler = &handle_signal;
        a.sa_flags = SA_NOCLDSTOP | SA_NODEFER;
        throw_errno_if_minus_one( boost::bind(&sigaction, SIGCHLD, &a, static_cast<struct sigaction *>(0))
                                , "cannot install SIGCHLD handler"
                                );
        sigset_t all;
        throw_errno_if_minus_one(boost::bind(&sigfillset, &all), "sigfillset()");
        throw_errno_if_minus_one(boost::bind(&sigprocmask, SIG_BLOCK, &all, &_orig_signalset), "sigprocmask()");
      }

      ~system()
      {
        throw_errno_if_minus_one( boost::bind(&sigprocmask, SIG_SETMASK, &_orig_signalset, static_cast<sigset_t *>(0))
                                , "sigprocmask()"
                                );
      }

      child_id execute(char const * const * argv, exit_handler const & f)
      {
        return execute(argv, environ, f);
      }

      child_id execute(char const * const * argv, char const * const * envp, exit_handler const & f)
      {
        BOOST_ASSERT(f);
        BOOST_ASSERT(argv); BOOST_ASSERT(argv[0]); BOOST_ASSERT(argv[0][0]);
        BOOST_ASSERT(envp);
        child_id const pid( throw_errno_if_minus_one(boost::bind(&fork), "cannot fork() new process") );
        if (pid == 0)             // child
        {
          execve(argv[0], const_cast<char **>(argv), const_cast<char **>(envp));
          std::cerr << "execve(" << argv[0] << "): " << system_error(errno).what() << std::endl;
          _Exit(1);
        }
        _hmap[pid] = f;           // parent
        return pid;
      }

      void kill(child_id pid)
      {
        if (::kill(pid, SIGKILL) == -1)
          if (errno != ESRCH)
            throw system_error(errno, "kill()");
      }

      void deliver_exit_codes()
      {
        while (!_hmap.empty())
        {
          int status;
          child_id const pid( throw_errno_if_minus_one(boost::bind(&waitpid, -1, &status, WNOHANG), "waitpid()") );
          if (pid == 0) return;
          handler_map::iterator const i( _hmap.find(pid) );
          if (i != _hmap.end())
          {
            exit_handler f;
            i->second.swap(f);
            _hmap.erase(i);
            f(status);
          }
        }
      }

      static void print_exit_code(std::ostream & os, system::exit_code ec)
      {
        if (WIFEXITED(ec))            os << "child process returned " << WEXITSTATUS(ec);
        else if (WIFSIGNALED(ec))     os << "child process terminated by signal " << WTERMSIG(ec)
                                         << (WCOREDUMP(ec) ? " (core dumped)" : "");
        else                          os << "child process returned unknown code " << ec;
      }

    protected:
      class signal_scope : private boost::noncopyable
      {
        sigset_t _orig_set;

      public:
        signal_scope()
        {
          sigset_t all;
          throw_errno_if_minus_one(boost::bind(&sigfillset, &all), "sigfillset()");
          throw_errno_if_minus_one(boost::bind(&sigprocmask, SIG_UNBLOCK, &all, &_orig_set), "sigprocmask()");
        }
        ~signal_scope()
        {
          throw_errno_if_minus_one(boost::bind(&sigprocmask, SIG_SETMASK, &_orig_set, static_cast<sigset_t *>(0)), "sigprocmask()");
        }
      };

    private:
      typedef std::map<child_id,exit_handler>   handler_map;
      handler_map                               _hmap;
      sigset_t                                  _orig_signalset;
      static void handle_signal(int) { }
    };

    // Co-operative multi-tasking scheduler.

    class core : public system
    {
    public:
      typedef boost::function0<void>                    task;

    private:
      typedef std::multimap<time_t,task>                task_queue;
      typedef std::pair<time_t,task_queue::iterator>    task_queue_entry;

    public:
      typedef task_queue_entry                          task_id;

      core() { update_core_time(); }

      task_id schedule(task const & t, second_t from_now = 0u)
      {
        BOOST_ASSERT(t);
        if (from_now > 0) update_core_time();
        task_queue::iterator const i( _tasks.insert(std::make_pair(_now + from_now, t)) );
        return std::make_pair(i->first, i);
      }

      time_t now() const { return _now; }

      bool cancel(task_id & tid)
      {
        if (tid.first > _now)
        {
          _tasks.erase(tid.second);
          tid.first = 0;
          return true;
        }
        else
          return false;
      }

      second_t run()
      {
        for (deliver_exit_codes(); !_tasks.empty(); deliver_exit_codes())
        {
          do
          {
            task_queue::iterator const i( _tasks.begin() );
            if (i->first > _now)
            {
              update_core_time();
              if (i->first > _now)
                return i->first - _now;
            }
            task t;
            t.swap(i->second);
            _tasks.erase(i);
            t();
          }
          while (!_tasks.empty());
        }
        return 0u;
      }

    protected:
      void update_core_time() { _now = std::time(0); }

    private:
      time_t            _now;
      task_queue        _tasks;
    };

    // I/O Event Dispatcher.

    class scheduler : public core
    {
    public:
      typedef int socket_id;

      explicit scheduler(unsigned int size_hint = 512u)
      {
        BOOST_ASSERT(size_hint <= static_cast<unsigned int>(std::numeric_limits<int>::max()));
        _epoll_fd = throw_errno_if_minus_one(boost::bind(&epoll_create, static_cast<int>(size_hint)), "epoll_create(2)");
      }

      ~scheduler()
      {
        throw_errno_if_minus_one(boost::bind(&close, _epoll_fd), "close() epoll socket");
      }

      void run()
      {
        for (second_t next_event( core::run() ); !_handlers.empty(); next_event = core::run())
        {
          epoll_event ev[128u];
          int rc;
          int const timeout( next_event ? static_cast<int>(next_event) * 1000 : -1 );
          {
            signal_scope const allow_signals;
            rc = epoll_wait(_epoll_fd, ev, sizeof(ev) / sizeof(epoll_event), timeout);
          }
          if (rc == -1)
          {
            if (errno == EINTR) continue;
            else                throw system_error(errno, "epoll_wait()");
          }
          for (int i(0); i != rc; ++i)
          {
            BOOST_ASSERT(ev[i].data.fd >= 0);
            BOOST_ASSERT(_handlers.find(ev[i].data.fd) != _handlers.end());
            handler & h( _handlers[ev[i].data.fd] );
            if ((ev[i].events & EPOLLIN)  && h.first)  schedule(h.first);
            if ((ev[i].events & EPOLLOUT) && h.second) schedule(h.second);
          }
        }
      }

      void register_socket(socket_id s)
      {
        TRACE_ARG1(s);
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) == _handlers.end());
        try
        {
          _handlers[s] = handler();
          epoll_event ev;
          ev.data.fd = s;
          ev.events  = 0;
          throw_errno_if_minus_one(boost::bind(&epoll_ctl, _epoll_fd, EPOLL_CTL_ADD, s, &ev), "registering new socket");
        }
        catch(...)
        {
          _handlers.erase(s);
          throw;
        }
      }

      void unregister_socket(socket_id s)
      {
        TRACE_ARG1(s);
        BOOST_ASSERT(s >= 0);
        handler_map::iterator const i( _handlers.find(s) );
        BOOST_ASSERT(i != _handlers.end());
        _handlers.erase(i);
        epoll_event ev;
        ev.data.fd = s;
        ev.events  = 0;
        throw_errno_if_minus_one(boost::bind(&epoll_ctl, _epoll_fd, EPOLL_CTL_DEL, s, &ev), "unregistering socket");
      }

      void on_input(socket_id s, task t)
      {
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) != _handlers.end());
        handler & h( _handlers[s] );
        h.first.swap(t);
        if ( static_cast<bool>(t) != static_cast<bool>(h.first) )
          modify_epoll(s, h, "cannot register on_input for socket");
      }

      void on_output(socket_id s, task t)
      {
        BOOST_ASSERT(s >= 0);
        BOOST_ASSERT(_handlers.find(s) != _handlers.end());
        handler & h( _handlers[s] );
        h.second.swap(t);
        if ( static_cast<bool>(t) != static_cast<bool>(h.second) )
          modify_epoll(s, h, "cannot register on_output for socket");
      }

    private:
      typedef std::pair<task,task>        handler;
      typedef std::map<socket_id,handler> handler_map;

      socket_id         _epoll_fd;
      handler_map       _handlers;

      void modify_epoll(socket_id s, handler const & h, std::string const & ctxid)
      {
        epoll_event ev;
        ev.data.fd = s;
        ev.events  = (h.first ? EPOLLIN : 0) | (h.second ? EPOLLOUT : 0);
        throw_errno_if_minus_one(boost::bind(&epoll_ctl, _epoll_fd, EPOLL_CTL_MOD, s, &ev), ctxid);
      }
    };

    class resolver : public scheduler
    {
    public:
      typedef std::string                               hostname;
      typedef std::vector<hostname>                     hostname_list;

      typedef std::string                               hostaddr;
      typedef std::vector<hostaddr>                     hostaddr_list;

      typedef std::pair<hostname,hostaddr_list>         mxname;
      typedef std::vector<mxname>                       mxname_list;

      typedef boost::function1<void, hostaddr_list *>   a_handler;
      typedef boost::function1<void, mxname_list *>     mx_handler;
      typedef boost::function1<void, hostname *>        ptr_handler;

      resolver() : _scheduled(false)
      {
        adns_initflags const flags(static_cast<adns_initflags>( adns_if_debug
                                                              | adns_if_noautosys
                                                              | adns_if_nosigpipe
                                                              | adns_if_checkc_freq
                                                              ));
        throw_rc_if_not_zero(adns_init(&_state, flags, static_cast<FILE*>(0)), "cannot initialize adns resolver");
        BOOST_ASSERT(_state);
        update_time();
      }

      ~resolver()
      {
        release_fds();
        adns_finish(_state);
      }

      void query_a(char const * owner, a_handler const & h)
      {
        BOOST_ASSERT(h);
        submit(owner, adns_r_a, 0, boost::bind(handleA, _1, h));
      }

      void query_a_no_cname(char const * owner, a_handler const & h)
      {
        BOOST_ASSERT(h);
        submit(owner, adns_r_a, 0 | adns_qf_cname_forbid, boost::bind(handleA, _1, h));
      }

      void query_mx(char const * owner, mx_handler const & h)
      {
        BOOST_ASSERT(h);
        submit(owner, adns_r_mx, 0, boost::bind(handleMX, _1, h));
      }

      void query_ptr(char const * owner, ptr_handler const & h)
      {
        BOOST_ASSERT(h);
        submit(owner, adns_r_ptr, 0, boost::bind(handlePTR, _1, h));
      }

    private:
      typedef boost::shared_ptr<adns_answer const>      answer;
      typedef boost::function1<void, answer>            callback;
      typedef std::map<adns_query,callback>             query_set;

      adns_state        _state;
      query_set         _qset;
      bool              _scheduled;

      void schedule_deliver()
      {
        check_consistency();
        if (!_scheduled)
        {
          schedule(boost::bind(&resolver::deliver, this));
          _scheduled = true;
        }
      }

      void release_fds()
      {
        cancel(_timeout);
        fd_set fdset;
        fdset.swap(_registered_fds);
        std::for_each(fdset.begin(), fdset.end(), boost::bind(&scheduler::unregister_socket, this, _1));
      }

      void submit(char const * owner, adns_rrtype rrtype, int flags, callback f)
      {
        schedule_deliver();
        adns_query qid;
        throw_rc_if_not_zero( adns_submit(_state, owner, rrtype, static_cast<adns_queryflags>(flags), static_cast<FILE*>(0), &qid)
                            , "submit DNS query"
                            );
        _qset[qid].swap(f);
      }

      void register_fds()
      {
        if (_qset.empty()) return release_fds();

        // Determine the file descriptors we have to probe for.

        boost::scoped_array<pollfd> fds( new pollfd[ADNS_POLLFDS_RECOMMENDED] );
        int nfds(ADNS_POLLFDS_RECOMMENDED);
        int timeout;
        update_time();
        for (int rc( ERANGE ); rc == ERANGE; /**/)
        {
          timeout = -1;
          rc = adns_beforepoll(_state, fds.get(), &nfds, &timeout, &_now);
          switch(rc)
          {
            case ERANGE:        BOOST_ASSERT(nfds > 0); fds.reset( new pollfd[nfds] ); break;
            case 0:             break;
            default:            throw system_error(rc, "adns_beforepoll()");
          }
        }
        BOOST_ASSERT(nfds >= 0);

        // Re-register the descriptors in scheduler.

        fd_set registered_fds;
        for (int i(0); i != nfds; ++i)
        {
          BOOST_ASSERT(fds[i].fd >= 0);
          BOOST_ASSERT(fds[i].events & (POLLIN | POLLOUT));
          TRACE_MSG("probe adns socket " << fds[i].fd);
          registered_fds.insert(fds[i].fd);
        }
        std::vector<int> fdset;
        std::set_difference( _registered_fds.begin(), _registered_fds.end()
                           , registered_fds.begin(),  registered_fds.end()
                           , std::back_insert_iterator< std::vector<int> >(fdset)
                           );
        std::for_each(fdset.begin(), fdset.end(), boost::bind(&scheduler::unregister_socket, this, _1));
        fdset.resize(0);
        std::set_difference( registered_fds.begin(),  registered_fds.end()
                           , _registered_fds.begin(), _registered_fds.end()
                           , std::back_insert_iterator< std::vector<int> >(fdset)
                           );
        std::for_each(fdset.begin(), fdset.end(), boost::bind(&scheduler::register_socket, this, _1));
        _registered_fds.swap(registered_fds);
        for (int i(0); i != nfds; ++i)
        {
          if (fds[i].events & POLLIN)
            on_input(fds[i].fd, boost::bind(&resolver::process_fd, this, &adns_processreadable, fds[i].fd));
          else
            on_input(fds[i].fd, scheduler::task());
          if (fds[i].events & POLLOUT)
            on_output(fds[i].fd, boost::bind(&resolver::process_fd, this, &adns_processwriteable, fds[i].fd));
          else
            on_output(fds[i].fd, scheduler::task());
        }
        cancel(_timeout);
        timeout /= 1000;
        if (timeout == 0)  process_timeout();
        if (timeout > 0)   _timeout = schedule(boost::bind(&resolver::process_timeout, this), timeout);
      }

      typedef std::set<int>     fd_set;
      typedef fd_set::iterator  fd_set_iterator;

      fd_set            _registered_fds;
      timeval           _now;
      core::task_id     _timeout;

      void update_time()
      {
        _now.tv_sec  = now();
        _now.tv_usec = 0;
      }

      void process_fd(int (*f)(adns_state, int, timeval const *), int fd)
      {
        TRACE_MSG("process adns socket" << fd);
        schedule_deliver();
        update_time();
        throw_rc_if_not_zero((*f)(_state, fd, &_now), "process DNS I/O");
      }

      void process_timeout()
      {
        TRACE_MSG("process adns timeouts");
        schedule_deliver();
        update_time();
        adns_processtimeouts(_state, &_now);
      }

      void deliver()
      {
        TRACE_MSG("deliver adns events");
        _scheduled = false;
        check_consistency();
        answer ans;
        for (callback f; /**/; f.clear(), ans.reset())
        {
          adns_query      qid(0);
          adns_answer *   a(0);
          int const       rc( adns_check(_state, &qid, &a, 0) );
          switch (rc)
          {
            case EINTR:   continue;
            case ESRCH:   BOOST_ASSERT(_qset.empty());  return release_fds();
            case EAGAIN:  BOOST_ASSERT(!_qset.empty()); return register_fds();
            case 0:       break;
            default:      throw system_error(rc, "adns_check()");
          }
          BOOST_ASSERT(a);
          ans.reset(a, &::free);
          query_set::iterator const i( _qset.find(qid) );
          BOOST_ASSERT(i != _qset.end());
          f.swap(i->second);
          _qset.erase(i);
          f(ans);
        }
        check_consistency();
      }

      void check_consistency() const
      {
#ifndef NDEBUG
        adns_forallqueries_begin(_state);
        for ( adns_query qid( adns_forallqueries_next(_state, 0) );
              qid != 0;
              qid = adns_forallqueries_next(_state, 0))
          BOOST_ASSERT(_qset.find(qid) != _qset.end());
#endif
      }

      static void handleA(answer a, a_handler h)
      {
        BOOST_ASSERT(a->type == adns_r_a);
        hostaddr_list hs;
        switch (a->status)
        {
          case adns_s_ok:
            BOOST_ASSERT(a->nrrs > 0);
            for (int i(0); i != a->nrrs; ++i)
            {
              char const * str( inet_ntoa(a->rrs.inaddr[i]) );
              BOOST_ASSERT(str);
              hs.push_back(str);
            }
            h(&hs);
            break;

          case adns_s_nxdomain:
          case adns_s_nodata:
            BOOST_ASSERT(a->nrrs == 0);
            h(&hs);
            break;

          default:
            BOOST_ASSERT(a->nrrs == 0);
            h(0);
            break;
        }
      }

      static void handleMX(answer a, mx_handler h)
      {
        BOOST_ASSERT(a->type == adns_r_mx);
        mxname_list mxs;
        std::multimap<int,adns_rr_hostaddr const *> mxmap;
        switch (a->status)
        {
          case adns_s_ok:
            BOOST_ASSERT(a->nrrs > 0);
            for (int i(0); i != a->nrrs; ++i)
            {
              adns_rr_inthostaddr const & addr( a->rrs.inthostaddr[i] );
              mxmap.insert(std::make_pair(addr.i, &addr.ha));
            }
            for (std::multimap<int,adns_rr_hostaddr const *>::iterator i(mxmap.begin()); i != mxmap.end(); ++i)
            {
              adns_rr_hostaddr const & addr( *i->second );
              hostaddr_list ha;
              if (addr.naddrs > 0)
                for (int j(0); j != addr.naddrs; ++j)
                  ha.push_back(inet_ntoa( addr.addrs[j].addr.inet.sin_addr ));
              mxs.push_back(std::make_pair(addr.host, ha));
            }
            h(&mxs);
            break;

          case adns_s_nxdomain:
          case adns_s_nodata:
            BOOST_ASSERT(a->nrrs == 0);
            h(&mxs);
            break;

          default:
            BOOST_ASSERT(a->nrrs == 0);
            h(0);
            break;
        }
      }

      static void handlePTR(answer a, ptr_handler h)
      {
        BOOST_ASSERT(a->type == adns_r_ptr);
        hostname ha;
        switch (a->status)
        {
          case adns_s_ok:
            BOOST_ASSERT(a->nrrs == 1);
            ha.assign(a->rrs.str[0]);
            h(&ha);
            break;

          case adns_s_nxdomain:
          case adns_s_nodata:
            BOOST_ASSERT(a->nrrs == 0);
            h(&ha);
            break;

          default:
            BOOST_ASSERT(a->nrrs == 0);
            h(0);
            break;
        }
      }

      static void throw_rc_if_not_zero(int rc, std::string const & ctx)
      {
        if (rc != 0) { system_error error(rc, ctx); throw error; }
      }
    };

    // Basic I/O.

    class basic_socket;
    typedef boost::shared_ptr<basic_socket> socket;

    class basic_socket : private boost::noncopyable
    {
    public:
      typedef scheduler::task                           task;
      typedef scheduler::task_id                        task_id;
      typedef scheduler::socket_id                      socket_id;
      typedef boost::function1<void, char *>            input_handler;
      typedef boost::function1<void, char const *>      output_handler;

      basic_socket(scheduler & io, socket_id fd) : _io(io), _sock(fd)
      {
        BOOST_ASSERT(fd >= 0);
        int const rc( fcntl(fd, F_GETFL, 0) );
        if (rc == -1) throw system_error(errno, "fcntl() failed to obtain socket flags");
        int const flags( rc | O_NONBLOCK );
        if (rc != flags)
          if (fcntl(fd, F_SETFL, flags) == -1)
            throw system_error(errno, "fcntl() failed to set socket flags");
        _io.register_socket(_sock);
      }

      ~basic_socket()
      {
        _io.unregister_socket(_sock);
        throw_errno_if_minus_one(boost::bind(&close, _sock), "close()");
      }

      void read(char * begin, char * end, input_handler const & f)
      {
        BOOST_ASSERT(begin < end);
        _io.on_input(_sock, boost::bind(&basic_socket::do_read, this, begin, end, f));
      }

      void write(char const * begin, char const * end, output_handler const & f)
      {
        BOOST_ASSERT(begin < end);
        _io.on_output(_sock, boost::bind(&basic_socket::do_write, this, begin, end, f));
      }

      task_id schedule(task const & f, second_t to = 0u)
      {
        return _io.schedule(f, to);
      }

      bool cancel(task_id tid)  { return _io.cancel(tid); }
      void cancel_input()       { _io.on_input(_sock, task()); }
      void cancel_output()      { _io.on_output(_sock, task()); }

      socket_id get_socket() const   { return _sock; }
      scheduler & get_io()           { return _io; }

    private:
      scheduler &       _io;
      socket_id const   _sock;

      void do_read(char * begin, char * end, input_handler f)
      {
        cancel_input();
        ssize_t const rc( ::read(_sock, begin, end - begin) );
        if (rc < 0) f(static_cast<char *>(0));
        else        f(begin + rc);
      }

      void do_write(char const * begin, char const * end, output_handler f)
      {
        cancel_output();
        ssize_t const rc( ::write(_sock, begin, end - begin) );
        if (rc < 0) f(static_cast<char *>(0));
        else        f(begin + rc);
      }
    };

    typedef boost::function<void (socket, sockaddr const *, socklen_t)> socket_handler;

    inline void accept_stream_socket(socket ls, socket_handler f)
    {
      sockaddr  addr;
      socklen_t len;
      socket    sock;
      for (int s( accept(ls->get_socket(), &addr, &len) ); s >= 0; s = accept(ls->get_socket(), &addr, &len))
      {
        try
        {
          sock.reset( new basic_socket(ls->get_io(), s) );
        }
        catch(...)
        {
          throw_errno_if_minus_one(boost::bind(&::close, s), "cannot close() listening socket");
          throw;
        }
        f(sock, &addr, len);
      }
      if (errno != EWOULDBLOCK && errno != EAGAIN)
        throw system_error(errno, "failed to accept() new connection");
    }

    inline socket accept_stream_socket(scheduler & io, char const * node, char const * service, socket_handler f)
    {
      boost::shared_ptr<addrinfo> _addr;
      socket s;
      addrinfo   hint = { AI_NUMERICHOST, 0, SOCK_STREAM, 0, 0u, NULL, NULL, NULL };
      addrinfo * addr;
      int const rc( getaddrinfo(node, service, &hint, &addr) );
      if (rc != 0) throw std::runtime_error( gai_strerror(rc) );
      _addr.reset(addr, &freeaddrinfo);
      while (addr && addr->ai_socktype != SOCK_STREAM)
        addr = addr->ai_next;
      if (!addr) throw std::runtime_error("address does not map to a stream socket endpoint");
      int const ls( throw_errno_if_minus_one(boost::bind(&::socket, addr->ai_family, addr->ai_socktype, addr->ai_protocol), "socket(2)") );
      try
      {
        s.reset( new basic_socket(io, ls) );
      }
      catch(...)
      {
        throw_errno_if_minus_one(boost::bind(&::close, ls), "cannot close() listening socket");
        throw;
      }
      throw_errno_if_minus_one(boost::bind(&::bind, ls, addr->ai_addr, addr->ai_addrlen), "bind(2)");
      throw_errno_if_minus_one(boost::bind(&::listen, ls, 16u), "listen(2)");
      io.on_input(ls, boost::bind(&accept_stream_socket, s, f));
      return s;
    }
  }
}

#endif // POSTMASTER_IO_HPP_2007_11_18
