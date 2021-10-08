template <class T = void_t<foo, foo>[i >> 1
				    ],
	  class U = T::template foo<bar, x>>
  void f(int x);

template <class T1, class T2>
  template <class U>
    constexpr
    inline
    void
    pair<T1, T2>::
      pair()
    : pair()
    {
      g();
    }

template <class T1, class T2>
  template <class U>
    constexpr
    SOME_MACRO
    void
    f();

template <bool _Valid = (x == 1)
			  && foo>
  void
  f()
  {
    return g<_Res>(_M_f, _Mu<_Bound_args>()
			   (std::get<_Indexes>(_M_bound_args), __args)...);
  }

int x = foo<bar<baz
		  + 2
	   >>;
int x = foo<bar<x,
		baz + 2>>;
int x = foo<x,
	    bar<baz + 2>>;

static_assert(1 < 2,
	      "huh?");
static_assert(x < 2,
	      "huh?");
static_assert(x < 2,
	      foo<1>());
// The following is just not worth the effort. '<' is ambiguous and it's
// unreasonable to disambiguate.
static_assert(x < 2,
		  "it's"
		  "complicated");

void
f();

int
  var;

ttype<T>
  var;

ttype<bar<T>>
  var;

_GLIBCXX_CONSTEXPR
void
f()
{
  N = N
	+ 1;
  n = n
	+ 1;
}

template <class T>
  _GLIBCXX_CONSTEXPR
  void
  f();

foo({1, 2, 3,
     4, 5, 6});

using blah = std::integral_constant<
	       bool, foo
		       && bar>;
using blah = std::integral_constant<
	       bool, foo &&
		       bar>;

where(V([](int i) { return i; }) < 2, V(0))
  .copy_to(mem);

template <class T>
  constexpr int foobar
    = a::x<y>::z
	|| sdlafkj<asdf>::asdf
	&& (xasdf
	      || y);

/**
 * Let
 * me do
 * some writing
 */

using all_udt2 = vir<1>::Typelist<1,
				  enable_if_t<foo <= bar>,
				  enable_if_t<(foo < bar)>,
				  enable_if_t<foo >= bar>,
				  enable_if_t<(foo > bar)>,
				  std::pair<int, foo<bar>>,
				  pair<int, int>
				 >;

using all_udt2 = vir<1>::Typelist<1, (size<foo<bar()>>
					+ 2,
				      1),
				  pair<int, int>
				 >;

using all_udt
  = vir<1, 2>::Typelist<
      enable_if_t<foo <= bar>,
      std::pair<int, foo<bar>,
		int, int>, std::pair<int,
				     float>,
      std::pair<char, std::pair<double, int>>, std::tuple<int, double, short>,
      std::tuple<int,
		 std::pair<unsigned, std::tuple<float, float, float>>,
		 int>>;

std::foo(
  x);

::std::foo(
  x);

f(x, ::blah<1, 2>::foo(
       x
	 + 1,
       y),
  x + ::vir::foo[1].x(
	blah)
 );

test_value<V>({}, {1000}, [](V x) {
  // blah
  if constexpr (foo)
    f();
});

f(asldfkjfun(x, [&]() {
    return x; //
  });

asldfkjfun(
  x,
  [&]() {
    return x; //
  },
  [&]() {
    return y; //
  },
  y
);

f(V(x, [&](auto i) {
    return x;
  }, [&](auto i) {
    return y;
  }),
  g(V([&](auto i) {
      return x;
    }));

auto x = [&]() {
  return 1;
}();
auto x = foobar([&]() {
	   return 1;
	 });

using all_udt_simd
  = remove_unusable_types<vir::expand_one<vir::Template<stdx::simd>,
					  vir::outer_product<all_udt,
#if !defined ABITYPES || ABITYPES == 0
							     all_native_abis
#elif 1
#ifdef foo
							     foo
#endif
#else
							     fixed_size_abi_list
#endif
			 >>>;

template <typename _Tp>
  concept __tuple_of_simd_like = requires(_Tp __a)
  {
    {foo} -> bar;
  };

template <typename _From, typename _To>
  concept __converts_to_higher_integer_rank
    = sizeof(_From) < sizeof(_To);

namespace __detail
{
  template <typename _Tp>
    concept __tuple_of_simd_like = requires(_Tp __a) {
      {std::get<0>(__a)}
	-> __simd_specialization;
      {};
    };
}

template <>
  std::array<int, 3> arr = {{
    1,
    f(g({1})),
    0.1232523847628347268347628375683648273648273462893746827346283746283746283,
    0.2232523847628347268347628375683648273648273462893746827346283746283746283,
    0.3232523847628347268347628375683648273648273462893746827346283746283746283,
    0.4232523847628347268347628375683648273648273462893746827346283746283746283,
    0.5232523847628347268347628375683648273648273462893746827346283746283746283,
    0.6232523847628347268347628375683648273648273462893746827346283746283746283,
    0.7232523847628347268347628375683648273648273462893746827346283746283746283,
    0.8232523847628347268347628375683648273648273462893746827346283746283746283,
    0.9232523847628347268347628375683648273648273462893746827346283746283746283,
    1.0232523847628347268347628375683648273648273462893746827346283746283746283,
    1.1232523847628347268347628375683648273648273462893746827346283746283746283,
    1.2232523847628347268347628375683648273648273462893746827346283746283746283,
    1.3232523847628347268347628375683648273648273462893746827346283746283746283,
    1.4232523847628347268347628375683648273648273462893746827346283746283746283,
    1.5232523847628347268347628375683648273648273462893746827346283746283746283,
    1.6232523847628347268347628375683648273648273462893746827346283746283746283,
    1.7232523847628347268347628375683648273648273462893746827346283746283746283,
    1.8232523847628347268347628375683648273648273462893746827346283746283746283,
    1.9232523847628347268347628375683648273648273462893746827346283746283746283,
    2.0232523847628347268347628375683648273648273462893746827346283746283746283,
    2.1232523847628347268347628375683648273648273462893746827346283746283746283,
    2.2232523847628347268347628375683648273648273462893746827346283746283746283,
    2.3232523847628347268347628375683648273648273462893746827346283746283746283,
    2.4232523847628347268347628375683648273648273462893746827346283746283746283,
    2.5232523847628347268347628375683648273648273462893746827346283746283746283,
    2.6232523847628347268347628375683648273648273462893746827346283746283746283,
    2.7232523847628347268347628375683648273648273462893746827346283746283746283,
    2.8232523847628347268347628375683648273648273462893746827346283746283746283,
    2.9232523847628347268347628375683648273648273462893746827346283746283746283,
    3.0232523847628347268347628375683648273648273462893746827346283746283746283,
    3.1232523847628347268347628375683648273648273462893746827346283746283746283,
    3.2232523847628347268347628375683648273648273462893746827346283746283746283,
    3.3232523847628347268347628375683648273648273462893746827346283746283746283,
    3.4232523847628347268347628375683648273648273462893746827346283746283746283,
    3.5232523847628347268347628375683648273648273462893746827346283746283746283,
    3.6232523847628347268347628375683648273648273462893746827346283746283746283,
    3.7232523847628347268347628375683648273648273462893746827346283746283746283,
    3.8232523847628347268347628375683648273648273462893746827346283746283746283,
    3.9232523847628347268347628375683648273648273462893746827346283746283746283,
    0.0232523847628347268347628375683648273648273462893746827346283746283746283,
    0.1232523847628347268347628375683648273648273462893746827346283746283746283,
  }};

f(0,
  1, f(1,
       2), {0, 1,
	    2, {3,
		4}
	   });

struct A
{
public:
  template <class T, int x>
    static T
    construct()
    {
      if constexpr (std::is_arithmetic_v<T>)
	return x;
      else if constexpr (stdx::__any_pair<T> || stdx::__any_tuple<T>)
	return stdx::__generate_from_n_evaluations<std::tuple_size_v<T>, T>(
		 [](auto i) {
		   return construct<std::tuple_element_t<i, T>, x + i * 11>();
		 });
      a = b ? c ? c
		: e
	    : foo ? g
		  : h + 1;
      bool x = sizeof...(Elements) == 1
		 && !is_same<foo, bar>;
      if (y)
	return sizeof...(Elements) == 1
		 && !is_same<foo, bar>;
      else if (x)
	return sizeof...(Elements) == 1
		 && !is_same<foo, bar>;
      a = f(xasdf
	      ? foo
	      : bar);
      __data(get<__i>(__ret)) = __k ? __data(get<__i>(__t))
				    : __data(get<__i>(__ret));
      a = xyz
	    + f<123>(g<123>);
      a = a > 5 ? a : b;
      foo();
      for (int i = 0; i < f((400 < g()),
			    blah);
	   i++)
	{
	  foo();
	}
      /*x*/ for ( int i = 0; i < f((400 < g()), // for()
				   blah);
		  ++i)
	      {
		foo();
	      }
      f(
	{0, 1,
	 2, 3
	},
	x);
      {
	f();
	f()
	  << "foo";
      }
      {
	f();
label:
	f()
	  << "foo";
	f() << "foo"
	    << "bar" <<
	    "baz";
	f()
	  << "foo"
	  << "bar" <<
	  "baz";
      }
      for (;;)
	x();
      y();

      xsdf = sladkfj && b(asdf,
			  asdfsd) +
	       asldfkj
	       && clksadfj(
		    x, y
		  )
	       == d;
      foo<bar>(1, []() { asldfkj; }) << x
				     << y;
    }

private:
  int _M_data;

public:
  template <typename _Flags>
    _GLIBCXX_SIMD_ALWAYS_INLINE
    simd(const value_type* __mem, _Flags)
    : simd([&](auto __i) { return __mem[__i]; }),
      _M_data(),
      _M_bar(3),
      _M_foo{1, {2, {3}}, f()}
    {}

  int _M_foo[2];

  simd(int)
  : public base<T, U>,
    private other<foo, bar>,
    protected for_good_measure<w<t<f>>>,
    _M_data("I'm good")
  {
    f();
  }

  simd()
  :_M_data(),
   _M_foo{}
  {}

  simd() : _M_data(),
	   _M_foo{}
  {}

  constexpr
  simd()
  noexcept(false)
  : _M_data()
  {
  }

  template <class U>
    void
    f(int x
	= int(),
      int y
	= 1)
    noexcept
    const
    &;

  simd_mask<T, Abi>
  operator==(simd a, simd b)
  const;

  auto
  operator==(simd a, simd b)
  -> simd::mask_type
  const;

  _GLIBCXX_SIMD_INTRINSIC friend constexpr
  _Base&
  __data(simd& __x)
  { return *this; }

  template <class T = void_t<foo, foo>[i >> 1
				      ],
	    class U = T::template foo<bar, x>>
    void f(int x)
    {
      _Impl::_S_set(__data(get<__j + 1>(*this), f(), g(f())), __i,
		    get<__j + 1>(__mem[__i]));
      g (1, 2,
	 true
	   ? 1
	   : 2,
	 f(1
	     + 1
	     == 2),
	 3, 4);
      if (foo)
	{
	  bar;
	  if constexpr(bar)
	    {
	      if(foo)
		foo;
	      {}
	    }
	}
      {
	fo;
      }
      foo;
      {{
      }}
      if(foo) {
      }
      if (foo)
	for (int i = 0; i < 10; ++i)
	  {
	    f(i);
	  }
      else if (x)
	if (y)
	  for (int i = 0; i < 10; ++i)
	    {
	      f(i);
	    }
      {{
	sdf;
      }}
      foo;
      std::foobarbaz(1, "Hallo Welt", "foo"
				      "foo",
		     "bar",
		     std::foobarbaz (1, 2
				    ),
		     y(1)
		       + z
		       + x,
		     x
		       .y()
		    );
      std::foobarbaz(
	1, "Hallo Welt", "foo"
			 "foo",
	"bar",
	std::foobarbaz (1, 2
		       ),
	y(1)
	  + z
	  + x,
	x
	  .y()
      );
      ++x;
    }

  template <class T>
    void g()
    {
      stdx::__execute_n_times<N>([&](auto i) {
	auto element = std::get<i>(x);
	using Vi = decltype(element);
	if constexpr (plain_simd_type<Vi>)
	  {
	    COMPARE(element, Vi([](auto j) { return i * 11 + j; }))
	      .on_failure(x);
	  }
	else
	  {
	    COMPARE(element,
		    Vi([](auto j) {
		      return
			construct<typename Vi::value_type, i * 11 + j>();
		    }));
	  }
	COMPARE(element, (construct<std::tuple_element_t<i, T>, i * 11>()))
	  .on_failure(x);
      });

      [&]<std::size_t... indices>(std::index_sequence<indices...>)
      {
	// recurse
	(test(std::get<indices>(x)), ...);
	// test operator+= on tuple member and compare
	([&](auto i) {
	  if constexpr (plain_simd_type<std::tuple_element_t<i, V>>)
	    {
	      V y = x;
	      std::get<i>(y) += 1;
	      COMPARE(x == y, M(false)).on_failure(x, y);
	      COMPARE(x != y, M(true)).on_failure(x, y);
	      COMPARE(x > y, M(false)).on_failure(x, y);
	      COMPARE(x >= y, M(false)).on_failure(x, y);
	      COMPARE(x < y, M(true)).on_failure(x, y);
	      COMPARE(x <= y, M(true)).on_failure(x, y);
	    }
	}(std::integral_constant<int, indices>()), ...);
      }(std::make_index_sequence<N>()
       );
    }
};

template <typename... Ts>
  class tuple
  : public inherited<0>,
    blubb,
    protected inherited2<1, 2>,
    private base
  {
  public:
    template <typename A, typename B>
      constexpr
      tuple(impl<A, B>&&, int x = int([]() { return 1; }()))
      : inherited<0>(x, std::move
			  (foo)),
	inherited2(std::move
		     (foo)),
	base(std::forward<A>
	       (blah)),
	blubb(x, std::forward<B>
		   (y))
      {}

    tuple() noexcept
    : inherited(x)
    {}

    tuple() noexcept(noexcept(foo<1>()))
    : inherited(x)
    {}
  };

f(x,
  (1 + 2) * 3);

template <class T>
  void f();
template <class T>
  void g();

template <>
  void
  f()
  {
#if FOO
    return;
#else
    return;
#endif
  }

template <class T>
  requires std::convertible_to<T, int>
  auto
  f(int x)
  noexcept
  -> decltype(x + 1)
  {
    return 0;
  }

namespace details
{
  /*
   * A comment
   */
  int x;
  namespace blah
  {
    /*
     * {sdf}
     */
    {
      int y;
    }
  }
  int z;
}

void ht()
{
  constexpr const char* ref
    = is_int<U>()
	? "void @1::@3::fun:4<T>() [with T = float]"
	: "void @1::@2<U>::fun:4<T>() [with T = float; U = char]";
  *a = 5
	 + 1;
}

namespace X
{
  void f()
  {
    if (x == 0)
      return 1;
    else
      return 2;
  }

#define foo(x) \
  if (x == 0)  \
    return 1;  \
  else         \
    return 2;

#define foo(x) \
  if (x == 0)  \
    return 1;  \
  else         \
    return 2;

#define foo(x) if (x == 0)  \
		 return 1;  \
	       else         \
		 return 2;

  void g();
}

// vim: noet sw=2 ts=8 tw=80 cc=81 indentexpr=GnuIndent()
