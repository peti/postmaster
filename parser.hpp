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

#ifndef POSTMASTER_PARSER_HPP_INCLUDED
#define POSTMASTER_PARSER_HPP_INCLUDED

#if 0                           // enable spirit debugging
#  define BOOST_SPIRIT_DEBUG
#  define BOOST_SPIRIT_DEBUG_OUT std::cerr
#endif

#include "postmaster.hpp"
#include "rfc2822/address.hpp"
#include <boost/spirit/phoenix/binders.hpp>

namespace spirit = boost::spirit ;

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
      using namespace boost::spirit;
      using namespace phoenix;
      using namespace rfc2822;

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

address_parser const address_p;

struct address_pattern_parser : public spirit::grammar<address_pattern_parser, address_closure::context_t>
{
  address_pattern_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> addr_pattern;

    definition(address_pattern_parser const & self)
    {
      using namespace boost::spirit;
      using namespace phoenix;
      using namespace rfc2822;

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

address_pattern_parser const address_pattern_p;

struct target_parser : public spirit::grammar<target_parser, address_closure::context_t>
{
  target_parser() { }

  struct target_id : public string
  {
    template <class Iterator>
    target_id(Iterator b, Iterator e)
    {
      string::push_back('<');
      string::insert(string::end(), b, e);
      string::push_back('>');
    };
  };

  struct target_parameter : public string
  {
    template <class Iterator>
    target_parameter(Iterator b, Iterator e) : string(++b, --e) { }
  };

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> target;

    definition(target_parser const & self)
    {
      using namespace boost::spirit;
      using namespace phoenix;
      using namespace rfc2822;

      target
        = word_p                            [ bind(&address::first)(self.val)  = construct_<target_id>(arg1, arg2) ]
          >> *wsp_p
          >> confix_p('(', *anychar_p, ')') [ bind(&address::second)(self.val) = construct_<target_parameter>(arg1, arg2) ]
        ;
      BOOST_SPIRIT_DEBUG_NODE(target);
    }

    spirit::rule<scannerT> const & start() const { return target; }
  };
};

target_parser const target_p;

struct route_closure : public spirit::closure<route_closure, route>
{
  member1 val;
};

struct route_lhs_impl
{
  template <typename Pair>
  struct result
  {
    typedef target & type;
  };
  template <typename Pair>
  target & operator() (Pair & p) const
  {
    return p.first;
  }
};

struct route_rhs_impl
{
  template <typename Pair>
  struct result
  {
    typedef target_list & type;
  };
  template <typename Pair>
  target_list & operator() (Pair & p) const
  {
    return p.second;
  }
};

phoenix::function<route_lhs_impl> const route_lhs = route_lhs_impl();
phoenix::function<route_rhs_impl> const route_rhs = route_rhs_impl();

struct push_back_impl
{
  template <typename Container, typename Item>
  struct result
  {
    typedef void type;
  };

  template <typename Container, typename Item>
  void operator()(Container& c, Item const& item) const
  {
    c.push_back(item);
  }
};

phoenix::function<push_back_impl> const push_back = push_back_impl();

struct route_parser : public spirit::grammar<route_parser, route_closure::context_t>
{
  route_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> route_p;

    definition(route_parser const & self)
    {
      using namespace boost::spirit;
      using namespace phoenix;
      using namespace rfc2822;

      route_p
        =  ( address_pattern_p                          [ route_lhs(self.val) = arg1 ]
           | ch_p('@')                                  [ route_lhs(self.val) = construct_<target>() ]
           )
             >> wsp_p                                   [ route_rhs(self.val) = construct_<target_list>() ]
             >> list_p( *wsp_p >> ( target_p            [ push_back(route_rhs(self.val), arg1) ]
                                  | address_pattern_p   [ push_back(route_rhs(self.val), arg1) ]
                                  )
                      , ','
                      )
        ;

      BOOST_SPIRIT_DEBUG_NODE(route_p);
    }

    spirit::rule<scannerT> const & start() const { return route_p; }
  };
};

route_parser const route_p;

#endif // POSTMASTER_PARSER_HPP_INCLUDED
