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
#include <boost/compatibility/cpp_c_headers/cerrno>

system_error::system_error()
: boost::system::system_error(errno, boost::system::errno_ecat)
{
}

system_error::system_error(string const & msg)
: boost::system::system_error(errno, boost::system::errno_ecat, msg)
{
}
