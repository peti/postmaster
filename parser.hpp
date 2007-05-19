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

#ifndef POSTMASTER_PARSER_HPP_INCLUDED
#define POSTMASTER_PARSER_HPP_INCLUDED

#if 0                           // enable spirit debugging
#  define BOOST_SPIRIT_DEBUG
#  define BOOST_SPIRIT_DEBUG_OUT std::cerr
#endif

#include "postmaster.hpp"
#include "rfc2822/address.hpp"
#include <boost/spirit/phoenix/binders.hpp>

using namespace rfc2822;

struct address_closure : public spirit::closure<address_closure, address>
{
  member1 val;
};

struct address_parser : public spirit::grammar<address_parser, address_closure::context_t>
{
  address_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> addr;

    definition(address_parser const & self)
    {
      using namespace spirit;
      using namespace phoenix;

      addr
        = local_part_p [bind(&address::first)(self.val)  = arg1]
          >> ch_p('@')
          >> domain_p  [bind(&address::second)(self.val) = arg1]
        ;

      BOOST_SPIRIT_DEBUG_NODE(addr);
    }

    spirit::rule<scannerT> const & start() const { return addr; }
  };
};

address_parser const  address_p;

struct address_pattern_parser : public spirit::grammar<address_pattern_parser, address_closure::context_t>
{
  address_pattern_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> addr_pattern;

    definition(address_pattern_parser const & self)
    {
      using namespace spirit;
      using namespace phoenix;

      addr_pattern
        =  (  ch_p('@') >> domain_p  [ bind(&address::second)(self.val) = arg1 ]
           |  address_p              [ self.val = arg1 ]
           |  local_part_p           [ bind(&address::first)(self.val)  = arg1 ] >> !ch_p('@')
           )
        ;

      BOOST_SPIRIT_DEBUG_NODE(addr_pattern);
    }

    spirit::rule<scannerT> const & start() const { return addr_pattern; }
  };
};

address_pattern_parser const  address_pattern_p;

#endif // POSTMASTER_PARSER_HPP_INCLUDED
