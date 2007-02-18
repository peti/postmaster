/*
 * Copyright (c) 2001-2007 Peter Simons <simons@cryp.to>
 *
 * This software is provided 'as-is', without any express or
 * implied warranty. In no event will the authors be held liable
 * for any damages arising from the use of this software.
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty
 * provided the copyright notice and this notice are preserved.
 */

#include "logging.hpp"
#include <cstdio>
// #include <iostream>
// #include <boost/log/log_impl.hpp>
// #include <boost/log/functions.hpp>
// #include "logging.hpp"
// #include "sanity/platform.hpp"
#include "boost/assert.hpp"

BOOST_DEFINE_LOG(Postmaster::logging::main, "postmaster.main")

static void write_to_cerr(std::string const & prefix, std::string const & msg)
{
  using namespace std;
  int const rc( fprintf(stderr, "[%s] %s\n", prefix.c_str(), msg.c_str()) );
  BOOST_ASSERT(rc == (prefix.size() + msg.size() + 4u));
}

SANITY_DLL_EXPORT void ioxx::logging::init_cerr()
{
  using namespace boost::logging;

  manipulate_logs("ioxx.*")
    .add_modifier(&prepend_prefix)
    .add_appender(&write_to_cerr);
  flush_log_cache();
}
