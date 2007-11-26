#include "esmtp.hpp"
#include "io.hpp"
#include <iostream>
#include <boost/tuple/tuple_io.hpp>

struct print
{
  void operator() (postmaster::io::system::exit_code e) const
  {
    postmaster::io::system::print_exit_code(std::cout, e);
    std::cout << std::endl;
  }

  void operator() (std::string const * str) const
  {
    std::cout << (str ? str->c_str() : "error") << std::endl;
  }

  void operator() (std::vector<std::string> const * vec) const
  {
    if (vec)
    {
      std::cout << "[ ";
      std::copy(vec->begin(), vec->end(), std::ostream_iterator<std::string>(std::cout, " "));
      std::cout << "]" << std::endl;
    }
    else
      std::cout << "error" << std::endl;
  }

  void operator() (std::vector< std::pair< std::string,std::vector<std::string> > > const * vec) const
  {
    using namespace postmaster::io;
    if (vec)
    {
      for (resolver::mxname_list::const_iterator i( vec->begin() ); i != vec->end(); ++i)
      {
        std::cout << "MX " << i->first << " [ ";
        std::copy(i->second.begin(), i->second.end(), std::ostream_iterator<std::string>(std::cout, " "));
        std::cout << "]" << std::endl;
      }
    }
    else
      std::cout << "error" << std::endl;
  }
};

void print_id(unsigned int id) { std::cout << "task id " << id << std::endl; }

inline std::ostream & operator<< (std::ostream & os, boost::spirit::parse_info<> const r)
{
   os << "parser: ";
   if (r.hit)
   {
     if (r.full) os << "full ";
     os << "hit (" << r.length << " bytes)";
   }
   else
     os << "miss";
   return os;
}

class forward : private boost::noncopyable
{
public:
  typedef postmaster::io::core::task_id         timeout_id;
  typedef postmaster::io::basic_socket          basic_socket;
  typedef postmaster::io::socket                socket;
  typedef std::vector<char>                     iobuf;
  typedef boost::shared_ptr<forward>            context;

  forward(socket sin, socket sout = socket())
  : _sin(sin), _sout(sout ? sout : sin), _buf(1024u)
  {
  }

  static void run(socket s, sockaddr const *, socklen_t)
  {
    context ctx( new forward(s) );
    run(ctx);
  }

  static void run(context ctx)
  {
    forward & self( *ctx );
    char * const begin( &self._buf[0] );
    char * const end( begin + self._buf.size() );
    self._timeout = self._sout->schedule(boost::bind(&basic_socket::cancel_input, self._sin), 10u);
    self._sin->read(begin, end, boost::bind(&handle_read, ctx, _1));
  }

private:
  socket        _sin, _sout;
  iobuf         _buf;
  char const *  _data_begin;
  char const *  _data_end;
  timeout_id    _timeout;

  static void handle_read(context ctx, char * read_end)
  {
    forward & self( *ctx );
    if (!self._sin->cancel(self._timeout)) return;
    self._data_begin = &self._buf[0];
    self._data_end   = read_end;
    if (self._data_end <= self._data_begin) return;
    self._timeout = self._sout->schedule(boost::bind(&basic_socket::cancel_output, self._sin), 10u);
    self._sout->write(self._data_begin, self._data_end, boost::bind(&handle_write, ctx, _1));
  }

  static void handle_write(context ctx, char const * write_end)
  {
    forward & self( *ctx );
    if (!self._sout->cancel(self._timeout)) return;
    if (!write_end) return;
    self._data_begin = write_end;
    if (self._data_begin == self._data_end)
      run(ctx);
    else
    {
      self._timeout = self._sout->schedule(boost::bind(&basic_socket::cancel_output, self._sout), 10u);
      self._sout->write(self._data_begin, self._data_end, boost::bind(&handle_write, ctx, _1));
    }
  }
};

int main(int, char**)
{
  using namespace std;
  using namespace postmaster;
  using namespace postmaster::io;
  using boost::bind;

  using namespace boost::spirit;
  using namespace phoenix;

  char const input[] = "EHLo write-ONLY.cryp.to\r\n\r\nnoop\r\nnoop fo bar \r\nnabc\r\n\r";

  esmtp::server serv;
  char const * r( serv(&input[0], &input[sizeof(input)-1]) );
  if (!r) cout << "syntax error" << endl;
  else    cout << "consumed " << r - &input[0] << " bytes, "
               << &input[sizeof(input)-1] - r << " left."
               << endl;

  resolver io;
  {
    typedef boost::shared_ptr<basic_socket> shared_socket;
    shared_socket sin( new basic_socket(io, STDIN_FILENO) );
    shared_socket sout( new basic_socket(io, STDOUT_FILENO) );
    boost::shared_ptr<forward> f( new forward(sin, sout) );
    forward::run(f);
  }
  {
    char const * const argv[] = { "/bin/env", 0 };
    char const * const envp[] = { "test-variable=test contents", 0 };
    io.execute(argv, envp, print());
  }
  {
    char const * argv[] = { "/usr/bin/sleep", 0, 0 };
    argv[1] = "1"; io.execute(argv, print());
    argv[1] = "2"; io.execute(argv, print());
    argv[1] = "3"; io.execute(argv, print());
    argv[1] = "4";
    io::system::child_id const pid( io.execute(argv, print()) );
    io.schedule(bind(&io::system::kill, &io, pid), 1u);
  }
  {
    io::socket ls( accept_stream_socket(io, 0, "8080", boost::bind(&forward::run, _1, _2, _3)) );
    io.schedule(bind(&scheduler::on_input, &io, ls->get_socket(), scheduler::task()), 30u);
    ls.reset();
  }
  io.schedule(bind(print_id, 3u), 1u);
  io.schedule(bind(print_id, 0u));
  io.schedule(bind(print_id, 4u), 3u);
  io.schedule(bind(print_id, 1u));
  io.schedule(bind(print_id, 2u));
  io.query_a("peti-ip.localhost", print());
  io.query_a_no_cname("peti-mx.localhost", print());
  io.query_mx("peti-ip.localhost", print());
  io.query_ptr("1.0.0.127.in-addr.arpa", print());

  for (;;)
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
