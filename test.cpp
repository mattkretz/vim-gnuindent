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
			  && foo
			  || foo
			  ^ x
			  and bar
			  or x
			  xor x>
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

using blah = xxxxx<
	       bool, foo
		       && bar,
	       foo
		 && bar,
	       bool, foo &&
		     bar,
	       foo &&
	       bar,
	       bool, foo &
		     bar,
	       foo &
	       bar,
	       bool, foo +
		       bar,
	       foo +
		 bar,
	       bool, foo *
		     bar,
	       foo *
	       bar,
	       bool, foo <=
		       bar,
	       foo <=
		 bar,
	       bool, foo >=
		       bar,
	       foo >=
		 bar,
	       bool, foo >>
		       bar,
	       foo >>
		 bar,
	     >;

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

test_value<V>({}, 1000, [](V x) {
  // blah
  if constexpr (foo)
    f();
});

test_value<V>({}, {1000}, [](V x) {
  // blah
  if constexpr (foo)
    f();
});

static inline constexpr std::size_t size
  = N > 0 ? []<int I>(auto x) constexpr {
	      return 1;
	    }
	  : []<std::size_t... Is>(std::index_sequence<Is...>) constexpr {
	      return std::max({ simdize_size_v<typename simdize_impl<
						 std::tuple_element_t<Is, Tup>, 0>::type>... });
	    }(std::make_index_sequence<std::tuple_size_v<Tup>>());

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
      __tmp = (__xn < __minn)
		? (__xn == 0 ? __fp_zero : __fp_subnormal)
		: __fp_normal;
      __tmp = (__xn < __minn)
		? __xn == 0 ? __fp_zero : __fp_subnormal
		: __fp_normal;

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

  constexpr
  simd()
  noexcept(false) [[foo, bar::baz(1)]]
  : _M_data()
  {}

  constexpr
  simd() [[foo, bar::baz(1)]]
  : _M_data()
  {}

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
    and requires (T x)
    {
      { x + x } -> same_as<T>;
    }
    or foo<T>
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

inline
void f()
{}

__attribute__((diagnose_as("fun:3")))
void h()
{}

namespace X
{
  __attribute__((diagnose_as("fun:3")))
  void h()
  {}

  [[gnu::diagnose_as("fun")]]
  void g()
  {}

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

#define VIR_STRUCT_GET_   \
  auto                    \
  to_tuple_ref(T &&obj)   \
  {                       \
    return 1;             \
  }                       \

if (TMPL_ARGS_DEPTH (template_args) > 1
      && DECL_CLASS_SCOPE_P (specialized_t))
  {
  }

void f(int i)
{
  switch (i)
  {
    case 1:
      g(i);
      return 0;
    case 2:
    case 3:
      ++i; // misaligned after two cases
      break;
    case 4:
    case 5:
      // indented by hand, but gets auto-indented when I type foo(
  }
}

for (int i = 0; i < len; ++i)
  try_return_args = try_return_args && TREE_CODE (TREE_VEC_ELT (t, i))
		      == TEMPLATE_TYPE_PARM;

if (TREE_CODE (t_i) == TEMPLATE_TYPE_PARM)
  gcc_assert (i == TEMPLATE_TYPE_IDX (t_i)
		&& level == TEMPLATE_TYPE_LEVEL (t_i));

template <class T>
  concept foo = requires { T{}; }
		  or requires { T{1}; };

template <class T>
  concept foo2
    = foo<T> and requires(T* (&x)(int))
    {
      T{};
    }
	or requires
    {
      T{1};
    };

__x = __binary_op(__x, _Base::template _M_make_simd<_Tp, _Np>(
			 __vector_permute<1, 0, 3, 2, 5, 4, 7, 6>(
			   __x._M_data)));

return _SimdImplNeon<simd_abi::_Neon<8>>::_S_reduce(
	 __y, static_cast<_BinaryOperation&&>(__binary_op));

return _SimdImplNeon<simd_abi::_Neon>::_S_reduce(
	 __y, static_cast<_BinaryOperation&&>(__binary_op));

detail::simd_for_each_prologue<stdx::resize_simd_t<1, V>>(
  fun, std::ranges::data(rng));

detail::simd_for_each_prologue<stdx::resize_simd_t<1, V>, write_back,
			       stdx::memory_alignment_v<V>>(
  fun, std::ranges::data(rng), to_process);

g(f([] () {
    bar();
  }));

[&](auto... chunks) {
  std::invoke(fun, chunks...);
}([&](auto ptr) {
    return V([&](auto j) {
	     return ptr[j];
	   });
  }(data_or_ptr(r) + i + (V::size() * Is)), ...
 );

[&](auto x) { f(x); }([&](auto ptr) {
			return V([&](auto j) {
				 return ptr[j];
			       });
		      }(foo)
		     );

count = std::count_if(vir::execution::simd, data.begin(), data.end(), [](auto v) {
	  if constexpr (std::is_floating_point_v<T>)
	    return fmod(v, T(2)) == T(1);
	});

template <auto... Options>
  struct simd_policy
  {
    static constexpr simd_policy
    prefer_aligned() requires(_prefers_aligned)
    { return {}; }

    template <int N>
      static constexpr simd_policy<Options..., detail::simd_policy_unroll_by<N>>
      unroll_by() requires(_unroll_by == 0)
      {
	static_assert(N > 1);
	return {};
      }

    template <std::size_t N>
      static constexpr simd_policy<Options..., detail::simd_policy_size<N>>
      prefer_size() requires(_size == 0)
      {
	static_assert(N > 0);
	return {};
      };
  };

constexpr const auto*
operator->() const;

template <typename _Tp>
  struct _IsValidSizeFor
  : __bool_constant<(_UsedBytes / sizeof(_Tp) > 1 && _UsedBytes % sizeof(_Tp) == 0
		       && _UsedBytes <= __sve_vectorized_size_bytes)>
  {};

void
_S_store(const _TV __v, bool* __mem)
{
  _GLIBCXX_SIMD_INT_PACK(_S_size<__value_type_of<_TV>>, _Is, {
    ((__mem[_Is] = __v[_Is]), ...);
  });
}

__factor =  __to_x86_intrin(__vec_builtin_type<unsigned, 4>{
			      unsigned(__f[0]), unsigned(__f[1]),
			      unsigned(__f[2]), unsigned(__f[3])});

__factor =  __to_x86_intrin(__vec_builtin_type<unsigned, 4>{ // foo
			      unsigned(__f[0]), unsigned(__f[1]),
			      unsigned(__f[2]), unsigned(__f[3])});

foo({1, 2, 3,
     4, 5, 6});

f(0,
  1, f(1,
       2), {0, 1,
	    2, {3,
		4}
	   });

std::uint_least64_t _M_signed_zeros : 1
#if __NO_SIGNED_ZEROS__
  = 0;
#else
  = 1;
#endif

return __vec_bitcast_trunc<_TV>(
	 (__vec_bitcast<_Tp>((__xh & short(0xff00)))
	    | __vec_bitcast<_Tp>(__vec_bitcast<unsigned short>(__xl) >> 8))
	   & ((__vec_bitcast<_Tp>(__iy) & char(0xf8)) == 0));

const Rep b_low = disjunct ? std::to_representation(b)
			   : std::to_representation(b) & ((Rep(1) << extra_bits) - 1);

template <typename _It, typename... _Flags>
  requires __detail::__loadstore_convertible_to<std::iter_value_t<_It>, value_type, _Flags...>
    and std::contiguous_iterator<_It>
  _GLIBCXX_SIMD_ALWAYS_INLINE constexpr
  explicit(not __detail::__value_preserving_convertible_to<_Up, value_type>
	     || __detail::__higher_rank_than<_Up, value_type>)
  basic_simd(_It __first, const mask_type& __k, simd_flags<_Flags...> __flags = {}) noexcept
  : _M_data(_RepSimd([&](int __i) {
	      const auto* __ptr
		= __flags.template _S_adjust_pointer<basic_simd>(std::to_address(__first));
	      return __k[__i] ? std::to_representation(__ptr[__i])
			      : std::to_representation(value_type());
	    }))
  {}

auto f()
{
  auto x = [&]<__detail::_SimdSizeType... _Is>
	     (__detail::_SimdIndexSequence<_Is...>) {
    return (... and (__k[_Is] != 0));
  }(__detail::_MakeSimdIndexSequence<__size>());
  return [&]<__detail::_SimdSizeType... _Is> [[__gnu__::__always_inline__]]
	   (__detail::_SimdIndexSequence<_Is...>) {
    return (... and (__k[_Is] != 0));
  }(__detail::_MakeSimdIndexSequence<__size>());
  return foo<(foo_bar<>)>([] (auto __i) {
	   return 0;
	 });
  return [&]<int a,
	     int b> () {
    return 0;
  }();
  if (foo())
    return []() {
      return 0;
    }();
  else if (foo()) [[likely]]
    return []() {
      return 0;
    }();
  else
    return []() {
      return 0;
    }();
  while (true)
    x = [] {
      return 0;
    }();
  auto foo = [&]<int a,
		 int b>
	       [[foo]]
	       () {
    return 0;
  };
  foo::operator<int,
		int>();
  const std::resize_simd_t<__size, _T0>
    __x01{__private_init,
	  [&]<_SimdSizeType... _Is, _SimdSizeType... _Js,
	      _SimdSizeType... _Ks>
	    [[__gnu__::__always_inline__,
	      foo]]
	    (_SimdIndexSequence<_Is...>, _SimdIndexSequence<_Js...>,
	     _SimdIndexSequence<_Ks...>) {
	    return x;
	  }
  };
  x = foo(cond ? z
	       : y,
	  x));

  if constexpr (true)
    return s{} | s{} & x{} + y{} -= z{}
	     + 1;
  else if (1)
    return x {} bitand y {} <=> z {}
	     + 1;
  else
    return 0;

  if (1) [[unlikely]]
    return 1;
  else if (2) [[likely]]
    return 2;
  else if (3) [[likely]]
    {
      try
	{
	  test_runner();
	}
      catch(const test::precondition_failure& fail)
	{
	  return EXIT_FAILURE;
	}
      catch(...)
	{ throw; }
    }
}

template <typename>
  auto
  iota_v = {};

template <typename>
  foo<bar>
  iota_v = {};

template <typename>
  foo<bar<baz>>
  iota_v = {};

template <typename>
  decltype(1)
  iota_v = {};

template <typename>
  auto&
  iota_v = {};

template <typename>
  auto&&
  iota_v = {};

template <typename>
  auto*
  iota_v = {};

template <typename>
  auto**
  iota_v = {};

#define Foo                                                               \
  template <class _Tp>                                                    \
    requires xxxx<decltype(declval<value_type>() __op declval<_Tp>()),    \
		  value_type>                                             \
    int                                                                   \
    foo()                                                                 \
    {}

f(fd.exponent < 10 || fd.mantissa != 1
    || fd.exponent/64 < (int)std::size(pow10_adjustment_tab));

{
  template <typename U>
    simple_tuple(U init)
    : ::bar::foo<[]<size_t... Is>(is<Is...>){}(make_is<4>())>::x<0> {init}
    {}

  template <typename U>
    simple_tuple(U init)
    : x<0> {init}
    {}

  constexpr
  simple_tuple() = default;
}

if consteval
  {
    if (foo)
      {
	x();
      }
    else if consteval
      {
	foo;
      }
  }

class foo
{
public:
  foo &
    x;
}

return std::ranges::equal(M(k), k)
	 ? std::ranges::equal(M(!k), !k)x ? Passed
					  : Fail2
	 : Fail1;

using _Vp [[foo]]
  = int;

template <int X = 0>
  using _Vp [[foo]]
    = int;

void f(auto tup)
{
  template for (constexpr auto x : tup)
    {
      std::print("{}\n", x);
    }
  template for (constexpr auto x : tup)
    std::print("{}\n", x);
  template for (constexpr auto x
		  : tup)
    template for (constexpr auto x : tup)
      std::print("{}\n", x);
}

// vim: noet sw=2 ts=8 tw=80
