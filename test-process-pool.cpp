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
#include <boost/function.hpp>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/unit_test.hpp>

class process
{
public:
  typedef int                                   exit_code;
  typedef boost::function<void (exit_code)>     exit_handler;

  void run(exit_handler const & f, char const * filepath, char const * const * argv)
  {
    run(f, filepath, argv, environ);
  }

  void run(exit_handler const & f, char const * filepath, char const * const * argv, char const * const * envp)
  {
    BOOST_ASSERT(f);
    BOOST_ASSERT(filepath); BOOST_ASSERT(filepath[0]);
    BOOST_ASSERT(argv); BOOST_ASSERT(argv[0]);
    BOOST_ASSERT(envp);
    pid_t const pid( fork() );
    switch (pid)
    {
      case -1:
        throw system_error("cannot fork() new process");

      case 0:                   // child
        if ( execve(filepath, const_cast<char **>(argv), const_cast<char **>(envp)) == -1 )
        {
          MSG_ERROR("execve(" << filepath << ") failed: " << system_error().what());
          std::terminate();
        }

      default:                  // parent
        _hmap[pid] = f;
    };
  }

  bool run_once()
  {
    if (_hmap.empty()) return false;
    int status;
    pid_t const pid( waitpid(-1, &status, 0) );
    switch (pid)
    {
      case -1:  throw system_error("waitpid() failed");
      case 0:   return false;
      default:  /* fall through */;
    }
    handler_map::iterator const i( _hmap.find(pid) );
    if (i != _hmap.end())
    {
      exit_handler f;
      i->second.swap(f);
      _hmap.erase(i);
      f(status);
    }
    return true;
  };

private:
  typedef pid_t                                 id_type;
  typedef std::map<id_type,exit_handler>        handler_map;
  handler_map                                   _hmap;
};

void print(process::exit_code ec)
{
  if (WIFEXITED(ec))            std::cout << "child process returned " << WEXITSTATUS(ec);
  else if (WIFSIGNALED(ec))     std::cout << "child process terminated by signal " << WTERMSIG(ec)
                                          << (WCOREDUMP(ec) ? " (core dumped)" : "");
  else                          std::cout << "child process returned unknown code " << ec;
  std::cout << std::endl;
}

BOOST_AUTO_TEST_CASE( test_process_pool )
{
  using namespace std;

  process procs;
  char const * const argv[] = { "/bin/env", 0 };
  char const * const envp[] = { "test-variable=test contents", 0 };

  for (unsigned int i(16u); i; --i)
    procs.run(print, argv[0], argv, envp);
  while (procs.run_once()) /**/;
}
