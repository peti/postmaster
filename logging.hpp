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

#ifndef POSTMASTER_LOGGING_HPP_INCLUDED
#define POSTMASTER_LOGGING_HPP_INCLUDED

#include <boost/log/log.hpp>

namespace Postmaster
{
  namespace logging
  {
    BOOST_DECLARE_LOG(main)
  }
}

// Abstract access to our logger.
#define LOGMSG(lvl) BOOST_LOGL(Postmaster::logging::main, lvl)

#define MSG_DEBUG() LOGMSG(Postmaster::logging::dbg)
#define MSG_INFO()  LOGMSG(Postmaster::logging::info)
#define MSG_ERROR() LOGMSG(Postmaster::logging::err)

#endif // POSTMASTER_LOGGING_HPP_INCLUDED
