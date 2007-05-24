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

#define PP_PHOENIX_DEFINE_TUPLE_ACCESSOR(NAME) \
  PP_PHOENIX_DEFINE_TRIVIAL_RECORD_ACCESSOR(NAME, typename Record::NAME ## _type &)

PP_PHOENIX_DEFINE_TUPLE_ACCESSOR(first);
PP_PHOENIX_DEFINE_TUPLE_ACCESSOR(second);

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
        = local_part_p [first(self.val) = arg1]
          >> ch_p('@')
          >> domain_p  [second(self.val) = arg1]
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
        =  (  ch_p('@') >> domain_p  [ second(self.val) = arg1 ]
           |  address_p              [ self.val = arg1 ]
           |  local_part_p           [ first(self.val)  = arg1 ] >> !ch_p('@')
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
        = word_p                            [ first(self.val)  = construct_<target_id>(arg1, arg2) ]
          >> confix_p('(', *anychar_p, ')') [ second(self.val) = construct_<target_parameter>(arg1, arg2) ]
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

#define PP_PHOENIX_DEFINE_METHOD_ACTOR(NAME, TYPE, METHOD)      \
  struct NAME ## _impl                                          \
  {                                                             \
    template <typename Container, typename Item>                \
    struct result                                               \
    {                                                           \
      typedef TYPE type;                                        \
    };                                                          \
    template <typename Container, typename Item>                \
    TYPE operator()(Container & c, Item const & item) const     \
    {                                                           \
      c.METHOD ( item );                                        \
    }                                                           \
  };                                                            \
  phoenix::function<NAME ## _impl> const NAME = NAME ## _impl()

#define PP_PHOENIX_DEFINE_TRIVIAL_METHOD_ACTOR(NAME) \
   PP_PHOENIX_DEFINE_METHOD_ACTOR(NAME, void, NAME)

PP_PHOENIX_DEFINE_TRIVIAL_METHOD_ACTOR(push_back);

struct route_parser : public spirit::grammar<route_parser, route_closure::context_t>
{
  route_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> route_p;
    spirit::subrule<0>     lhs_p;
    spirit::subrule<1>     rhs_p;

    definition(route_parser const & self)
    {
      using namespace boost::spirit;
      using namespace phoenix;
      using namespace rfc2822;

      route_p =
        (
          lhs_p
            =  lexeme_d[ address_pattern_p [ first(self.val) = arg1 ]  |  ch_p('@') ]
            >> list_p(rhs_p, ',')

        , rhs_p
            = lexeme_d
            [  target_p            [ push_back(second(self.val), arg1) ]
            |  address_pattern_p   [ push_back(second(self.val), arg1) ]
            ]
        );

      BOOST_SPIRIT_DEBUG_NODE(route_p);
    }

    spirit::rule<scannerT> const & start() const { return route_p; }
  };
};

route_parser const route_p;

#endif // POSTMASTER_PARSER_HPP_INCLUDED
