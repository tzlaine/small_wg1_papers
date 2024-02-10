// g++-13 -std=c++23 conditionally_borrowed.cpp -o borrowed

#include <array>
#include <iostream>
#include <ranges>
#include <sstream>
#include <vector>

// Some subsequent operations on 'it' will reach back into 'v' for some if
// 'it' needs to know the end of the v.base_.  Even though we made a copy of
// ranges::end(v) -- 'last' -- we are not protected.

namespace std::ranges::z {
  template<typename _F>
    constexpr bool __tidy_func =
      is_trivially_copyable_v<_F> && sizeof(_F) <= sizeof(void*);
  template<typename _V>
    constexpr bool __tidy_view =
      is_trivially_copyable_v<_V> && sizeof(_V) <= sizeof(void*) * 2;

  template<input_range... _Vs>
    requires (view<_Vs> && ...) && (sizeof...(_Vs) > 0)
  class zip_view : public view_interface<zip_view<_Vs...>>
  {
    tuple<_Vs...> _M_views;

    template<bool> class _Iterator;
    template<bool> class _Sentinel;

  public:
    zip_view() = default;

    constexpr explicit
    zip_view(_Vs... __views)
      : _M_views(std::move(__views)...)
    { }

    constexpr auto
    begin() requires (!(__detail::__simple_view<_Vs> && ...))
    { return _Iterator<false>(__detail::__tuple_transform(ranges::begin, _M_views)); }

    constexpr auto
    begin() const requires (range<const _Vs> && ...)
    { return _Iterator<true>(__detail::__tuple_transform(ranges::begin, _M_views)); }

    constexpr auto
    end() requires (!(__detail::__simple_view<_Vs> && ...))
    {
      if constexpr (!__detail::__zip_is_common<_Vs...>)
        return _Sentinel<false>(__detail::__tuple_transform(ranges::end, _M_views));
      else if constexpr ((random_access_range<_Vs> && ...))
        return begin() + iter_difference_t<_Iterator<false>>(size());
      else
        return _Iterator<false>(__detail::__tuple_transform(ranges::end, _M_views));
    }

    constexpr auto
    end() const requires (range<const _Vs> && ...)
    {
      if constexpr (!__detail::__zip_is_common<const _Vs...>)
        return _Sentinel<true>(__detail::__tuple_transform(ranges::end, _M_views));
      else if constexpr ((random_access_range<const _Vs> && ...))
        return begin() + iter_difference_t<_Iterator<true>>(size());
      else
        return _Iterator<true>(__detail::__tuple_transform(ranges::end, _M_views));
    }

    constexpr auto
    size() requires (sized_range<_Vs> && ...)
    {
      return std::apply([](auto... sizes) {
	using _CT = __detail::__make_unsigned_like_t<common_type_t<decltype(sizes)...>>;
	return ranges::min({_CT(sizes)...});
      }, __detail::__tuple_transform(ranges::size, _M_views));
    }

    constexpr auto
    size() const requires (sized_range<const _Vs> && ...)
    {
      return std::apply([](auto... sizes) {
	using _CT = __detail::__make_unsigned_like_t<common_type_t<decltype(sizes)...>>;
	return ranges::min({_CT(sizes)...});
      }, __detail::__tuple_transform(ranges::size, _M_views));
    }
  };

  template<typename... _Rs>
    zip_view(_Rs&&...) -> zip_view<std::views::all_t<_Rs>...>;

  template<input_range... _Vs>
    requires (view<_Vs> && ...) && (sizeof...(_Vs) > 0)
  template<bool _Const>
  class zip_view<_Vs...>::_Iterator
    : public __detail::__zip_view_iter_cat<_Const, _Vs...>
  {
    __detail::__tuple_or_pair_t<iterator_t<__detail::__maybe_const_t<_Const, _Vs>>...> _M_current;

    constexpr explicit
    _Iterator(decltype(_M_current) __current)
      : _M_current(std::move(__current))
    { }

    static auto
    _S_iter_concept()
    {
      if constexpr (__detail::__all_random_access<_Const, _Vs...>)
	return random_access_iterator_tag{};
      else if constexpr (__detail::__all_bidirectional<_Const, _Vs...>)
	return bidirectional_iterator_tag{};
      else if constexpr (__detail::__all_forward<_Const, _Vs...>)
	return forward_iterator_tag{};
      else
	return input_iterator_tag{};
    }

    template<copy_constructible _Fp, input_range... _Ws>
      requires (view<_Ws> && ...) && (sizeof...(_Ws) > 0) && is_object_v<_Fp>
	&& regular_invocable<_Fp&, range_reference_t<_Ws>...>
	&& std::__detail::__can_reference<invoke_result_t<_Fp&, range_reference_t<_Ws>...>>
      friend class zip_transform_view;

  public:
    // iterator_category defined in __zip_view_iter_cat
    using iterator_concept = decltype(_S_iter_concept());
    using value_type
      = __detail::__tuple_or_pair_t<range_value_t<__detail::__maybe_const_t<_Const, _Vs>>...>;
    using difference_type
      = common_type_t<range_difference_t<__detail::__maybe_const_t<_Const, _Vs>>...>;

    _Iterator() = default;

    constexpr
    _Iterator(_Iterator<!_Const> __i)
      requires _Const
	&& (convertible_to<iterator_t<_Vs>,
			   iterator_t<__detail::__maybe_const_t<_Const, _Vs>>> && ...)
      : _M_current(std::move(__i._M_current))
    { }

    constexpr auto
    operator*() const
    {
      auto __f = [](auto& __i) -> decltype(auto) {
	return *__i;
      };
      return __detail::__tuple_transform(__f, _M_current);
    }

    constexpr _Iterator&
    operator++()
    {
      __detail::__tuple_for_each([](auto& __i) { ++__i; }, _M_current);
      return *this;
    }

    constexpr void
    operator++(int)
    { ++*this; }

    constexpr _Iterator
    operator++(int)
      requires __detail::__all_forward<_Const, _Vs...>
    {
      auto __tmp = *this;
      ++*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator--()
      requires __detail::__all_bidirectional<_Const, _Vs...>
    {
      __detail::__tuple_for_each([](auto& __i) { --__i; }, _M_current);
      return *this;
    }

    constexpr _Iterator
    operator--(int)
      requires __detail::__all_bidirectional<_Const, _Vs...>
    {
      auto __tmp = *this;
      --*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator+=(difference_type __x)
      requires __detail::__all_random_access<_Const, _Vs...>
    {
      auto __f = [&]<typename _It>(_It& __i) {
	__i += iter_difference_t<_It>(__x);
      };
      __detail::__tuple_for_each(__f, _M_current);
      return *this;
    }

    constexpr _Iterator&
    operator-=(difference_type __x)
      requires __detail::__all_random_access<_Const, _Vs...>
    {
      auto __f = [&]<typename _It>(_It& __i) {
	__i -= iter_difference_t<_It>(__x);
      };
      __detail::__tuple_for_each(__f, _M_current);
      return *this;
    }

    constexpr auto
    operator[](difference_type __n) const
      requires __detail::__all_random_access<_Const, _Vs...>
    {
      auto __f = [&]<typename _It>(_It& __i) -> decltype(auto) {
	return __i[iter_difference_t<_It>(__n)];
      };
      return __detail::__tuple_transform(__f, _M_current);
    }

    friend constexpr bool
    operator==(const _Iterator& __x, const _Iterator& __y)
      requires (equality_comparable<iterator_t<__detail::__maybe_const_t<_Const, _Vs>>> && ...)
    {
      if constexpr (__detail::__all_bidirectional<_Const, _Vs...>)
	return __x._M_current == __y._M_current;
      else
	return [&]<size_t... _Is>(index_sequence<_Is...>) {
	  return ((std::get<_Is>(__x._M_current) == std::get<_Is>(__y._M_current)) || ...);
	}(make_index_sequence<sizeof...(_Vs)>{});
    }

    friend constexpr auto
    operator<=>(const _Iterator& __x, const _Iterator& __y)
      requires __detail::__all_random_access<_Const, _Vs...>
    { return __x._M_current <=> __y._M_current; }

    friend constexpr _Iterator
    operator+(const _Iterator& __i, difference_type __n)
      requires __detail::__all_random_access<_Const, _Vs...>
    {
      auto __r = __i;
      __r += __n;
      return __r;
    }

    friend constexpr _Iterator
    operator+(difference_type __n, const _Iterator& __i)
      requires __detail::__all_random_access<_Const, _Vs...>
    {
      auto __r = __i;
      __r += __n;
      return __r;
    }

    friend constexpr _Iterator
    operator-(const _Iterator& __i, difference_type __n)
      requires __detail::__all_random_access<_Const, _Vs...>
    {
      auto __r = __i;
      __r -= __n;
      return __r;
    }

    friend constexpr difference_type
    operator-(const _Iterator& __x, const _Iterator& __y)
      requires (sized_sentinel_for<iterator_t<__detail::__maybe_const_t<_Const, _Vs>>,
				   iterator_t<__detail::__maybe_const_t<_Const, _Vs>>> && ...)
    {
      return [&]<size_t... _Is>(index_sequence<_Is...>) {
	return ranges::min({difference_type(std::get<_Is>(__x._M_current)
					    - std::get<_Is>(__y._M_current))...},
			   ranges::less{},
			   [](difference_type __i) {
			     return __detail::__to_unsigned_like(__i < 0 ? -__i : __i);
			   });
      }(make_index_sequence<sizeof...(_Vs)>{});
    }

    friend constexpr auto
    iter_move(const _Iterator& __i)
    { return __detail::__tuple_transform(ranges::iter_move, __i._M_current); }

    friend constexpr void
    iter_swap(const _Iterator& __l, const _Iterator& __r)
      requires (indirectly_swappable<iterator_t<__detail::__maybe_const_t<_Const, _Vs>>> && ...)
    {
      [&]<size_t... _Is>(index_sequence<_Is...>) {
	(ranges::iter_swap(std::get<_Is>(__l._M_current), std::get<_Is>(__r._M_current)), ...);
      }(make_index_sequence<sizeof...(_Vs)>{});
    }

    friend class zip_view;
  };

  template<input_range... _Vs>
    requires (view<_Vs> && ...) && (sizeof...(_Vs) > 0)
  template<bool _Const>
  class zip_view<_Vs...>::_Sentinel
  {
    __detail::__tuple_or_pair_t<sentinel_t<__detail::__maybe_const_t<_Const, _Vs>>...> _M_end;

    constexpr explicit
    _Sentinel(decltype(_M_end) __end)
      : _M_end(__end)
    { }

    friend class zip_view;

  public:
    _Sentinel() = default;

    constexpr
    _Sentinel(_Sentinel<!_Const> __i)
      requires _Const
	&& (convertible_to<sentinel_t<_Vs>,
			   sentinel_t<__detail::__maybe_const_t<_Const, _Vs>>> && ...)
      : _M_end(std::move(__i._M_end))
    { }

    template<bool _OtherConst>
      requires (sentinel_for<sentinel_t<__detail::__maybe_const_t<_Const, _Vs>>,
			     iterator_t<__detail::__maybe_const_t<_OtherConst, _Vs>>> && ...)
    friend constexpr bool
    operator==(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    {
      return [&]<size_t... _Is>(index_sequence<_Is...>) {
	return ((std::get<_Is>(__x._M_current) == std::get<_Is>(__y._M_end)) || ...);
      }(make_index_sequence<sizeof...(_Vs)>{});
    }

    template<bool _OtherConst>
      requires (sized_sentinel_for<sentinel_t<__detail::__maybe_const_t<_Const, _Vs>>,
				   iterator_t<__detail::__maybe_const_t<_OtherConst, _Vs>>> && ...)
    friend constexpr auto
    operator-(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    {
      using _Ret
	= common_type_t<range_difference_t<__detail::__maybe_const_t<_OtherConst, _Vs>>...>;
      return [&]<size_t... _Is>(index_sequence<_Is...>) {
	return ranges::min({_Ret(std::get<_Is>(__x._M_current) - std::get<_Is>(__y._M_end))...},
			   ranges::less{},
			   [](_Ret __i) {
			     return __detail::__to_unsigned_like(__i < 0 ? -__i : __i);
			   });
      }(make_index_sequence<sizeof...(_Vs)>{});
    }

    template<bool _OtherConst>
      requires (sized_sentinel_for<sentinel_t<__detail::__maybe_const_t<_Const, _Vs>>,
				   iterator_t<__detail::__maybe_const_t<_OtherConst, _Vs>>> && ...)
    friend constexpr auto
    operator-(const _Sentinel& __y, const _Iterator<_OtherConst>& __x)
    { return -(__x - __y); }
  };

  namespace views
  {
    namespace __detail
    {
      template<typename... _Ts>
	concept __can_zip_view
      = requires { zip_view<std::views::all_t<_Ts>...>(std::declval<_Ts>()...); };
    }

    struct _Zip
    {
      template<typename... _Ts>
	requires (sizeof...(_Ts) == 0 || __detail::__can_zip_view<_Ts...>)
	constexpr auto
	operator() [[nodiscard]] (_Ts&&... __ts) const
	{
	  if constexpr (sizeof...(_Ts) == 0)
            return std::views::empty<tuple<>>;
	  else
            return zip_view<std::views::all_t<_Ts>...>(std::forward<_Ts>(__ts)...);
	}
    };

    inline constexpr _Zip zip;
  }

  template<copy_constructible _Fp, input_range... _Vs>
    requires (view<_Vs> && ...) && (sizeof...(_Vs) > 0) && is_object_v<_Fp>
      && regular_invocable<_Fp&, range_reference_t<_Vs>...>
      && std::__detail::__can_reference<invoke_result_t<_Fp&, range_reference_t<_Vs>...>>
  class zip_transform_view : public view_interface<zip_transform_view<_Fp, _Vs...>>
  {
    [[no_unique_address]] __detail::__box<_Fp> _M_fun;
    zip_view<_Vs...> _M_zip;

    using _InnerView = zip_view<_Vs...>;

    template<bool _Const>
      using __ziperator = iterator_t<__detail::__maybe_const_t<_Const, _InnerView>>;

    template<bool _Const>
      using __zentinel = sentinel_t<__detail::__maybe_const_t<_Const, _InnerView>>;

    template<bool _Const>
      using _Base = __detail::__maybe_const_t<_Const, _InnerView>;

    template<bool _Const>
      struct __iter_cat
      { };

    template<bool _Const>
      requires forward_range<_Base<_Const>>
      struct __iter_cat<_Const>
      {
      private:
	static auto
	_S_iter_cat()
	{
	  using __detail::__maybe_const_t;
	  using __detail::__range_iter_cat;
	  using _Res = invoke_result_t<__maybe_const_t<_Const, _Fp>&,
				       range_reference_t<__maybe_const_t<_Const, _Vs>>...>;
	  if constexpr (!is_lvalue_reference_v<_Res>)
	    return input_iterator_tag{};
	  else if constexpr ((derived_from<__range_iter_cat<_Vs, _Const>,
					   random_access_iterator_tag> && ...))
	    return random_access_iterator_tag{};
	  else if constexpr ((derived_from<__range_iter_cat<_Vs, _Const>,
					   bidirectional_iterator_tag> && ...))
	    return bidirectional_iterator_tag{};
	  else if constexpr ((derived_from<__range_iter_cat<_Vs, _Const>,
					   forward_iterator_tag> && ...))
	    return forward_iterator_tag{};
	  else
	    return input_iterator_tag{};
	}
      public:
	using iterator_category = decltype(_S_iter_cat());
      };

    template<bool> class _Iterator;
    template<bool> class _Sentinel;

  public:
    zip_transform_view() = default;

    constexpr explicit
    zip_transform_view(_Fp __fun, _Vs... __views)
      : _M_fun(std::move(__fun)), _M_zip(std::move(__views)...)
    { }

    constexpr auto
    begin()
    { return _Iterator<false>(*this, _M_zip.begin()); }

    constexpr auto
    begin() const
      requires range<const _InnerView>
	&& regular_invocable<const _Fp&, range_reference_t<const _Vs>...>
    { return _Iterator<true>(*this, _M_zip.begin()); }

    constexpr auto
    end()
    {
      if constexpr (common_range<_InnerView>)
	return _Iterator<false>(*this, _M_zip.end());
      else
	return _Sentinel<false>(_M_zip.end());
    }

    constexpr auto
    end() const
      requires range<const _InnerView>
	&& regular_invocable<const _Fp&, range_reference_t<const _Vs>...>
    {
      if constexpr (common_range<const _InnerView>)
	return _Iterator<true>(*this, _M_zip.end());
      else
	return _Sentinel<true>(_M_zip.end());
    }

    constexpr auto
    size() requires sized_range<_InnerView>
    { return _M_zip.size(); }

    constexpr auto
    size() const requires sized_range<const _InnerView>
    { return _M_zip.size(); }
  };

  template<class _Fp, class... Rs>
    zip_transform_view(_Fp, Rs&&...) -> zip_transform_view<_Fp, std::views::all_t<Rs>...>;

  template<copy_constructible _Fp, input_range... _Vs>
    requires (view<_Vs> && ...) && (sizeof...(_Vs) > 0) && is_object_v<_Fp>
      && regular_invocable<_Fp&, range_reference_t<_Vs>...>
      && std::__detail::__can_reference<invoke_result_t<_Fp&, range_reference_t<_Vs>...>>
  template<bool _Const>
  class zip_transform_view<_Fp, _Vs...>::_Iterator : public __iter_cat<_Const>
  {
    using _Parent = __detail::__maybe_const_t<_Const, zip_transform_view>;

    using _F_access =
      conditional_t<__tidy_func<_Fp>, __detail::__box<_Fp>, _Parent*>;

    [[no_unique_address]] _F_access _M_f_access;

    __ziperator<_Const> _M_inner;

    constexpr const _Fp& __f() const
    {
        if constexpr (__tidy_func<_Fp>)
            return *_M_f_access;
        else
            return *_M_f_access->_M_fun;
    }

    constexpr
    _Iterator(const _F_access& __access, __ziperator<_Const> __inner)
      requires __tidy_func<_Fp>
      : _M_f_access(__access),
        _M_inner(std::move(__inner))
    { }

    constexpr
    _Iterator(_Parent& __parent, __ziperator<_Const> __inner)
      : _M_f_access([&]{
          if constexpr (__tidy_func<_Fp>)
            return __parent._M_fun;
          else
            return std::__addressof(__parent);
        }()),
        _M_inner(std::move(__inner))
    { }

    friend class zip_transform_view;

  public:
    // iterator_category defined in zip_transform_view::__iter_cat
    using iterator_concept = typename __ziperator<_Const>::iterator_concept;
    using value_type
      = remove_cvref_t<invoke_result_t<__detail::__maybe_const_t<_Const, _Fp>&,
				       range_reference_t<__detail::__maybe_const_t<_Const, _Vs>>...>>;
    using difference_type = range_difference_t<_Base<_Const>>;

    _Iterator() = default;

    constexpr
    _Iterator(_Iterator<!_Const> __i)
      requires _Const && convertible_to<__ziperator<false>, __ziperator<_Const>>
      : _M_f_access(__i._M_f_access), _M_inner(std::move(__i._M_inner))
    { }

    constexpr decltype(auto)
    operator*() const
    {
      return std::apply([&](const auto&... __iters) -> decltype(auto) {
        return std::__invoke(__f(), *__iters...);
      }, _M_inner._M_current);
    }

    constexpr _Iterator&
    operator++()
    {
      ++_M_inner;
      return *this;
    }

    constexpr void
    operator++(int)
    { ++*this; }

    constexpr _Iterator
    operator++(int) requires forward_range<_Base<_Const>>
    {
      auto __tmp = *this;
      ++*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator--() requires bidirectional_range<_Base<_Const>>
    {
      --_M_inner;
      return *this;
    }

    constexpr _Iterator
    operator--(int) requires bidirectional_range<_Base<_Const>>
    {
      auto __tmp = *this;
      --*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator+=(difference_type __x) requires random_access_range<_Base<_Const>>
    {
      _M_inner += __x;
      return *this;
    }

    constexpr _Iterator&
    operator-=(difference_type __x) requires random_access_range<_Base<_Const>>
    {
      _M_inner -= __x;
      return *this;
    }

    constexpr decltype(auto)
    operator[](difference_type __n) const requires random_access_range<_Base<_Const>>
    {
      return std::apply([&]<typename... _Is>(const _Is&... __iters) -> decltype(auto) {
        return std::__invoke(__f(), __iters[iter_difference_t<_Is>(__n)]...);
      }, _M_inner._M_current);
    }

    friend constexpr bool
    operator==(const _Iterator& __x, const _Iterator& __y)
      requires equality_comparable<__ziperator<_Const>>
    { return __x._M_inner == __y._M_inner; }

    friend constexpr auto
    operator<=>(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base<_Const>>
    { return __x._M_inner <=> __y._M_inner; }

    friend constexpr _Iterator
    operator+(const _Iterator& __i, difference_type __n)
      requires random_access_range<_Base<_Const>>
    { return _Iterator(__i._M_f_access, __i._M_inner + __n); }

    friend constexpr _Iterator
    operator+(difference_type __n, const _Iterator& __i)
      requires random_access_range<_Base<_Const>>
    { return _Iterator(__i._M_f_access, __i._M_inner + __n); }

    friend constexpr _Iterator
    operator-(const _Iterator& __i, difference_type __n)
      requires random_access_range<_Base<_Const>>
    { return _Iterator(__i._M_f_access, __i._M_inner - __n); }

    friend constexpr difference_type
    operator-(const _Iterator& __x, const _Iterator& __y)
      requires sized_sentinel_for<__ziperator<_Const>, __ziperator<_Const>>
    { return __x._M_inner - __y._M_inner; }
  };

  template<copy_constructible _Fp, input_range... _Vs>
    requires (view<_Vs> && ...) && (sizeof...(_Vs) > 0) && is_object_v<_Fp>
      && regular_invocable<_Fp&, range_reference_t<_Vs>...>
      && std::__detail::__can_reference<invoke_result_t<_Fp&, range_reference_t<_Vs>...>>
  template<bool _Const>
  class zip_transform_view<_Fp, _Vs...>::_Sentinel
  {
    __zentinel<_Const> _M_inner;

    constexpr explicit
    _Sentinel(__zentinel<_Const> __inner)
      : _M_inner(__inner)
    { }

    friend class zip_transform_view;

  public:
    _Sentinel() = default;

    constexpr
    _Sentinel(_Sentinel<!_Const> __i)
      requires _Const && convertible_to<__zentinel<false>, __zentinel<_Const>>
      : _M_inner(std::move(__i._M_inner))
    { }

    template<bool _OtherConst>
      requires sentinel_for<__zentinel<_Const>, __ziperator<_OtherConst>>
    friend constexpr bool
    operator==(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    { return __x._M_inner == __y._M_inner; }

    template<bool _OtherConst>
      requires sized_sentinel_for<__zentinel<_Const>, __ziperator<_OtherConst>>
    friend constexpr range_difference_t<__detail::__maybe_const_t<_OtherConst, _InnerView>>
    operator-(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    { return __x._M_inner - __y._M_inner; }

    template<bool _OtherConst>
      requires sized_sentinel_for<__zentinel<_Const>, __ziperator<_OtherConst>>
    friend constexpr range_difference_t<__detail::__maybe_const_t<_OtherConst, _InnerView>>
    operator-(const _Sentinel& __x, const _Iterator<_OtherConst>& __y)
    { return __x._M_inner - __y._M_inner; }
  };

  namespace views
  {
    namespace __detail
    {
      template<typename _Fp, typename... _Ts>
	concept __can_zip_transform_view
	  = requires { zip_transform_view(std::declval<_Fp>(), std::declval<_Ts>()...); };
    }

    struct _ZipTransform
    {
      template<typename _Fp, typename... _Ts>
	requires (sizeof...(_Ts) == 0) || __detail::__can_zip_transform_view<_Fp, _Ts...>
	constexpr auto
	operator() [[nodiscard]] (_Fp&& __f, _Ts&&... __ts) const
	{
	  if constexpr (sizeof...(_Ts) == 0)
	    return std::views::empty<decay_t<invoke_result_t<decay_t<_Fp>&>>>;
	  else
	    return zip_transform_view(std::forward<_Fp>(__f), std::forward<_Ts>(__ts)...);
	}
    };

    inline constexpr _ZipTransform zip_transform;
  }

  template<forward_range _Vp, size_t _Nm>
    requires view<_Vp> && (_Nm > 0)
  class adjacent_view : public view_interface<adjacent_view<_Vp, _Nm>>
  {
    _Vp _M_base = _Vp();

    template<bool> class _Iterator;
    template<bool> class _Sentinel;

    struct __as_sentinel
    { };

  public:
    adjacent_view() requires default_initializable<_Vp> = default;

    constexpr explicit
    adjacent_view(_Vp __base)
      : _M_base(std::move(__base))
    { }

    constexpr auto
    begin() requires (!__detail::__simple_view<_Vp>)
    { return _Iterator<false>(ranges::begin(_M_base), ranges::end(_M_base)); }

    constexpr auto
    begin() const requires range<const _Vp>
    { return _Iterator<true>(ranges::begin(_M_base), ranges::end(_M_base)); }

    constexpr auto
    end() requires (!__detail::__simple_view<_Vp>)
    {
      if constexpr (common_range<_Vp>)
	return _Iterator<false>(__as_sentinel{}, ranges::begin(_M_base), ranges::end(_M_base));
      else
	return _Sentinel<false>(ranges::end(_M_base));
    }

    constexpr auto
    end() const requires range<const _Vp>
    {
      if constexpr (common_range<const _Vp>)
	return _Iterator<true>(__as_sentinel{}, ranges::begin(_M_base), ranges::end(_M_base));
      else
	return _Sentinel<true>(ranges::end(_M_base));
    }

    constexpr auto
    size() requires sized_range<_Vp>
    {
      using _ST = decltype(ranges::size(_M_base));
      using _CT = common_type_t<_ST, size_t>;
      auto __sz = static_cast<_CT>(ranges::size(_M_base));
      __sz -= std::min<_CT>(__sz, _Nm - 1);
      return static_cast<_ST>(__sz);
    }

    constexpr auto
    size() const requires sized_range<const _Vp>
    {
      using _ST = decltype(ranges::size(_M_base));
      using _CT = common_type_t<_ST, size_t>;
      auto __sz = static_cast<_CT>(ranges::size(_M_base));
      __sz -= std::min<_CT>(__sz, _Nm - 1);
      return static_cast<_ST>(__sz);
    }
  };

  template<forward_range _Vp, size_t _Nm>
    requires view<_Vp> && (_Nm > 0)
  template<bool _Const>
  class adjacent_view<_Vp, _Nm>::_Iterator
  {
    using _Base = __detail::__maybe_const_t<_Const, _Vp>;
    array<iterator_t<_Base>, _Nm> _M_current = array<iterator_t<_Base>, _Nm>();

    constexpr
    _Iterator(iterator_t<_Base> __first, sentinel_t<_Base> __last)
    {
      for (auto& __i : _M_current)
	{
	  __i = __first;
	  ranges::advance(__first, 1, __last);
	}
    }

    constexpr
    _Iterator(__as_sentinel, iterator_t<_Base> __first, iterator_t<_Base> __last)
    {
      if constexpr (!bidirectional_range<_Base>)
	for (auto& __it : _M_current)
	  __it = __last;
      else
	for (size_t __i = 0; __i < _Nm; ++__i)
	  {
	    _M_current[_Nm - 1 - __i] = __last;
	    ranges::advance(__last, -1, __first);
	  }
    }

    static auto
    _S_iter_concept()
    {
      if constexpr (random_access_range<_Base>)
	return random_access_iterator_tag{};
      else if constexpr (bidirectional_range<_Base>)
	return bidirectional_iterator_tag{};
      else
	return forward_iterator_tag{};
    }

    friend class adjacent_view;

    template<forward_range _Wp, copy_constructible _Fp, size_t _Mm>
      requires view<_Wp> && (_Mm > 0) && is_object_v<_Fp>
        && regular_invocable<__detail::__unarize<_Fp&, _Mm>, range_reference_t<_Wp>>
        && std::__detail::__can_reference<invoke_result_t<__detail::__unarize<_Fp&, _Mm>,
							 range_reference_t<_Wp>>>
      friend class adjacent_transform_view;

  public:
    using iterator_category = input_iterator_tag;
    using iterator_concept = decltype(_S_iter_concept());
    using value_type = conditional_t<_Nm == 2,
				     pair<range_value_t<_Base>, range_value_t<_Base>>,
				     __detail::__repeated_tuple<range_value_t<_Base>, _Nm>>;
    using difference_type = range_difference_t<_Base>;

    _Iterator() = default;

    constexpr
    _Iterator(_Iterator<!_Const> __i)
      requires _Const && convertible_to<iterator_t<_Vp>, iterator_t<_Base>>
    {
      for (size_t __j = 0; __j < _Nm; ++__j)
	_M_current[__j] = std::move(__i._M_current[__j]);
    }

    constexpr auto
    operator*() const
    {
      auto __f = [](auto& __i) -> decltype(auto) { return *__i; };
      return __detail::__tuple_transform(__f, _M_current);
    }

    constexpr _Iterator&
    operator++()
    {
      for (auto& __i : _M_current)
	++__i;
      return *this;
    }

    constexpr _Iterator
    operator++(int)
    {
      auto __tmp = *this;
      ++*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator--() requires bidirectional_range<_Base>
    {
      for (auto& __i : _M_current)
	--__i;
      return *this;
    }

    constexpr _Iterator
    operator--(int) requires bidirectional_range<_Base>
    {
      auto __tmp = *this;
      --*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator+=(difference_type __x)
      requires random_access_range<_Base>
    {
      for (auto& __i : _M_current)
	__i += __x;
      return *this;
    }

    constexpr _Iterator&
    operator-=(difference_type __x)
      requires random_access_range<_Base>
    {
      for (auto& __i : _M_current)
	__i -= __x;
      return *this;
    }

    constexpr auto
    operator[](difference_type __n) const
      requires random_access_range<_Base>
    {
      auto __f = [&](auto& __i) -> decltype(auto) { return __i[__n]; };
      return __detail::__tuple_transform(__f, _M_current);
    }

    friend constexpr bool
    operator==(const _Iterator& __x, const _Iterator& __y)
    { return __x._M_current.back() == __y._M_current.back(); }

    friend constexpr bool
    operator<(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return __x._M_current.back() < __y._M_current.back(); }

    friend constexpr bool
    operator>(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return __y < __x; }

    friend constexpr bool
    operator<=(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return !(__y < __x); }

    friend constexpr bool
    operator>=(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return !(__x < __y); }

    friend constexpr auto
    operator<=>(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
	&& three_way_comparable<iterator_t<_Base>>
    { return __x._M_current.back() <=> __y._M_current.back(); }

    friend constexpr _Iterator
    operator+(const _Iterator& __i, difference_type __n)
      requires random_access_range<_Base>
    {
      auto __r = __i;
      __r += __n;
      return __r;
    }

    friend constexpr _Iterator
    operator+(difference_type __n, const _Iterator& __i)
      requires random_access_range<_Base>
    {
      auto __r = __i;
      __r += __n;
      return __r;
    }

    friend constexpr _Iterator
    operator-(const _Iterator& __i, difference_type __n)
      requires random_access_range<_Base>
    {
      auto __r = __i;
      __r -= __n;
      return __r;
    }

    friend constexpr difference_type
    operator-(const _Iterator& __x, const _Iterator& __y)
      requires sized_sentinel_for<iterator_t<_Base>, iterator_t<_Base>>
    { return __x._M_current.back() - __y._M_current.back(); }

    friend constexpr auto
    iter_move(const _Iterator& __i)
    { return __detail::__tuple_transform(ranges::iter_move, __i._M_current); }

    friend constexpr void
    iter_swap(const _Iterator& __l, const _Iterator& __r)
      requires indirectly_swappable<iterator_t<_Base>>
    {
      for (size_t __i = 0; __i < _Nm; __i++)
	ranges::iter_swap(__l._M_current[__i], __r._M_current[__i]);
    }
  };

  template<forward_range _Vp, size_t _Nm>
    requires view<_Vp> && (_Nm > 0)
  template<bool _Const>
  class adjacent_view<_Vp, _Nm>::_Sentinel
  {
    using _Base = __detail::__maybe_const_t<_Const, _Vp>;

    sentinel_t<_Base> _M_end = sentinel_t<_Base>();

    constexpr explicit
    _Sentinel(sentinel_t<_Base> __end)
      : _M_end(__end)
    { }

    friend class adjacent_view;

  public:
    _Sentinel() = default;

    constexpr
    _Sentinel(_Sentinel<!_Const> __i)
      requires _Const && convertible_to<sentinel_t<_Vp>, sentinel_t<_Base>>
      : _M_end(std::move(__i._M_end))
    { }

    template<bool _OtherConst>
      requires sentinel_for<sentinel_t<_Base>,
			    iterator_t<__detail::__maybe_const_t<_OtherConst, _Vp>>>
    friend constexpr bool
    operator==(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    { return __x._M_current.back() == __y._M_end; }

    template<bool _OtherConst>
      requires sized_sentinel_for<sentinel_t<_Base>,
				  iterator_t<__detail::__maybe_const_t<_OtherConst, _Vp>>>
    friend constexpr range_difference_t<__detail::__maybe_const_t<_OtherConst, _Vp>>
    operator-(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    { return __x._M_current.back() - __y._M_end; }

    template<bool _OtherConst>
      requires sized_sentinel_for<sentinel_t<_Base>,
				  iterator_t<__detail::__maybe_const_t<_OtherConst, _Vp>>>
    friend constexpr range_difference_t<__detail::__maybe_const_t<_OtherConst, _Vp>>
    operator-(const _Sentinel& __y, const _Iterator<_OtherConst>& __x)
    { return __y._M_end - __x._M_current.back(); }
  };

  namespace views
  {
    namespace __detail
    {
      template<size_t _Nm, typename _Range>
	concept __can_adjacent_view
	  = requires { adjacent_view<std::views::all_t<_Range>, _Nm>(std::declval<_Range>()); };
    }

    template<size_t _Nm>
      struct _Adjacent : std::views::__adaptor::_RangeAdaptorClosure
      {
	template<viewable_range _Range>
	  requires (_Nm == 0) || __detail::__can_adjacent_view<_Nm, _Range>
	  constexpr auto
	  operator() [[nodiscard]] (_Range&& __r) const
	  {
	    if constexpr (_Nm == 0)
	      return std::views::empty<tuple<>>;
	    else
	      return adjacent_view<std::views::all_t<_Range>, _Nm>(std::forward<_Range>(__r));
	  }
      };

    template<size_t _Nm>
      inline constexpr _Adjacent<_Nm> adjacent;

    inline constexpr auto pairwise = adjacent<2>;
  }

  template<forward_range _Vp, copy_constructible _Fp, size_t _Nm>
   requires view<_Vp> && (_Nm > 0) && is_object_v<_Fp>
     && regular_invocable<__detail::__unarize<_Fp&, _Nm>, range_reference_t<_Vp>>
     && std::__detail::__can_reference<invoke_result_t<__detail::__unarize<_Fp&, _Nm>,
						       range_reference_t<_Vp>>>
  class adjacent_transform_view : public view_interface<adjacent_transform_view<_Vp, _Fp, _Nm>>
  {
    [[no_unique_address]] __detail::__box<_Fp> _M_fun;
    adjacent_view<_Vp, _Nm> _M_inner;

    using _InnerView = adjacent_view<_Vp, _Nm>;

    template<bool _Const>
      using _InnerIter = iterator_t<__detail::__maybe_const_t<_Const, _InnerView>>;

    template<bool _Const>
      using _InnerSent = sentinel_t<__detail::__maybe_const_t<_Const, _InnerView>>;

    template<bool> class _Iterator;
    template<bool> class _Sentinel;

  public:
    adjacent_transform_view() = default;

    constexpr explicit
    adjacent_transform_view(_Vp __base, _Fp __fun)
      : _M_fun(std::move(__fun)), _M_inner(std::move(__base))
    { }

    constexpr auto
    begin()
    { return _Iterator<false>(*this, _M_inner.begin()); }

    constexpr auto
    begin() const
      requires range<const _InnerView>
	&& regular_invocable<__detail::__unarize<const _Fp&, _Nm>,
			     range_reference_t<const _Vp>>
    { return _Iterator<true>(*this, _M_inner.begin()); }

    constexpr auto
    end()
    {
      if constexpr (common_range<_InnerView>)
        return _Iterator<false>(*this, _M_inner.end());
      else
        return _Sentinel<false>(_M_inner.end());
    }

    constexpr auto
    end() const
      requires range<const _InnerView>
	&& regular_invocable<__detail::__unarize<const _Fp&, _Nm>,
			     range_reference_t<const _Vp>>
    {
      if constexpr (common_range<const _InnerView>)
        return _Iterator<true>(*this, _M_inner.end());
      else
        return _Sentinel<true>(_M_inner.end());
    }

    constexpr auto
    size() requires sized_range<_InnerView>
    { return _M_inner.size(); }

    constexpr auto
    size() const requires sized_range<const _InnerView>
    { return _M_inner.size(); }
  };

  template<forward_range _Vp, copy_constructible _Fp, size_t _Nm>
   requires view<_Vp> && (_Nm > 0) && is_object_v<_Fp>
     && regular_invocable<__detail::__unarize<_Fp&, _Nm>, range_reference_t<_Vp>>
     && std::__detail::__can_reference<invoke_result_t<__detail::__unarize<_Fp&, _Nm>,
						       range_reference_t<_Vp>>>
  template<bool _Const>
  class adjacent_transform_view<_Vp, _Fp, _Nm>::_Iterator
  {
    using _Parent = __detail::__maybe_const_t<_Const, adjacent_transform_view>;
    using _Base = __detail::__maybe_const_t<_Const, _Vp>;

    using _F_access =
      conditional_t<__tidy_func<_Fp>, __detail::__box<_Fp>, _Parent*>;

    [[no_unique_address]] _F_access _M_f_access;

    _InnerIter<_Const> _M_inner;

    constexpr const _Fp& __f() const
    {
        if constexpr (__tidy_func<_Fp>)
            return *_M_f_access;
        else
            return *_M_f_access->_M_fun;
    }

    constexpr
    _Iterator(const _F_access& __access, _InnerIter<_Const> __inner)
      requires __tidy_func<_Fp>
      : _M_f_access(__access),
        _M_inner(std::move(__inner))
    { }

    constexpr
    _Iterator(_Parent& __parent, _InnerIter<_Const> __inner)
      : _M_f_access([&] {
          if constexpr (__tidy_func<_Fp>)
            return __parent._M_fun;
          else
            return std::__addressof(__parent);
        }()),
        _M_inner(std::move(__inner))
    { }

    static auto
    _S_iter_cat()
    {
      using __detail::__maybe_const_t;
      using __detail::__unarize;
      using _Res = invoke_result_t<__unarize<__maybe_const_t<_Const, _Fp>&, _Nm>,
				   range_reference_t<_Base>>;
      using _Cat = typename iterator_traits<iterator_t<_Base>>::iterator_category;
      if constexpr (!is_lvalue_reference_v<_Res>)
	return input_iterator_tag{};
      else if constexpr (derived_from<_Cat, random_access_iterator_tag>)
	return random_access_iterator_tag{};
      else if constexpr (derived_from<_Cat, bidirectional_iterator_tag>)
	return bidirectional_iterator_tag{};
      else if constexpr (derived_from<_Cat, forward_iterator_tag>)
	return forward_iterator_tag{};
      else
	return input_iterator_tag{};
    }

    friend class adjacent_transform_view;

  public:
    using iterator_category = decltype(_S_iter_cat());
    using iterator_concept = typename _InnerIter<_Const>::iterator_concept;
    using value_type
      = remove_cvref_t<invoke_result_t
		       <__detail::__unarize<__detail::__maybe_const_t<_Const, _Fp>&, _Nm>,
			range_reference_t<_Base>>>;
    using difference_type = range_difference_t<_Base>;

    _Iterator() = default;

    constexpr
    _Iterator(_Iterator<!_Const> __i)
      requires _Const && convertible_to<_InnerIter<false>, _InnerIter<_Const>>
      : _M_f_access(__i._M_f_access), _M_inner(std::move(__i._M_inner))
    { }

    constexpr decltype(auto)
    operator*() const
    {
      return std::apply([&](const auto&... __iters) -> decltype(auto) {
        return std::__invoke(__f(), *__iters...);
      }, _M_inner._M_current);
    }

    constexpr _Iterator&
    operator++()
    {
      ++_M_inner;
      return *this;
    }

    constexpr _Iterator
    operator++(int)
    {
      auto __tmp = *this;
      ++*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator--() requires bidirectional_range<_Base>
    {
      --_M_inner;
      return *this;
    }

    constexpr _Iterator
    operator--(int) requires bidirectional_range<_Base>
    {
      auto __tmp = *this;
      --*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator+=(difference_type __x) requires random_access_range<_Base>
    {
      _M_inner += __x;
      return *this;
    }

    constexpr _Iterator&
    operator-=(difference_type __x) requires random_access_range<_Base>
    {
      _M_inner -= __x;
      return *this;
    }

    constexpr decltype(auto)
    operator[](difference_type __n) const requires random_access_range<_Base>
    {
      return std::apply([&](const auto&... __iters) -> decltype(auto) {
        return std::__invoke(__f(), __iters[__n]...);
      }, _M_inner._M_current);
    }

    friend constexpr bool
    operator==(const _Iterator& __x, const _Iterator& __y)
    { return __x._M_inner == __y._M_inner; }

    friend constexpr bool
    operator<(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return __x._M_inner < __y._M_inner; }

    friend constexpr bool
    operator>(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return __x._M_inner > __y._M_inner; }

    friend constexpr bool
    operator<=(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return __x._M_inner <= __y._M_inner; }

    friend constexpr bool
    operator>=(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base>
    { return __x._M_inner >= __y._M_inner; }

    friend constexpr auto
    operator<=>(const _Iterator& __x, const _Iterator& __y)
      requires random_access_range<_Base> &&
      three_way_comparable<_InnerIter<_Const>>
    { return __x._M_inner <=> __y._M_inner; }

    friend constexpr _Iterator
    operator+(const _Iterator& __i, difference_type __n)
      requires random_access_range<_Base>
    { return _Iterator(__i._M_f_access, __i._M_inner + __n); }

    friend constexpr _Iterator
    operator+(difference_type __n, const _Iterator& __i)
      requires random_access_range<_Base>
    { return _Iterator(__i._M_f_access, __i._M_inner + __n); }

    friend constexpr _Iterator
    operator-(const _Iterator& __i, difference_type __n)
      requires random_access_range<_Base>
    { return _Iterator(__i._M_f_access, __i._M_inner - __n); }

    friend constexpr difference_type
    operator-(const _Iterator& __x, const _Iterator& __y)
      requires sized_sentinel_for<_InnerIter<_Const>, _InnerIter<_Const>>
    { return __x._M_inner - __y._M_inner; }
  };

  template<forward_range _Vp, copy_constructible _Fp, size_t _Nm>
   requires view<_Vp> && (_Nm > 0) && is_object_v<_Fp>
     && regular_invocable<__detail::__unarize<_Fp&, _Nm>, range_reference_t<_Vp>>
     && std::__detail::__can_reference<invoke_result_t<__detail::__unarize<_Fp&, _Nm>,
						       range_reference_t<_Vp>>>
  template<bool _Const>
  class adjacent_transform_view<_Vp, _Fp, _Nm>::_Sentinel
  {
    _InnerSent<_Const> _M_inner;

    constexpr explicit
    _Sentinel(_InnerSent<_Const> __inner)
      : _M_inner(__inner)
    { }

    friend class adjacent_transform_view;

  public:
    _Sentinel() = default;

    constexpr
    _Sentinel(_Sentinel<!_Const> __i)
      requires _Const && convertible_to<_InnerSent<false>, _InnerSent<_Const>>
      : _M_inner(std::move(__i._M_inner))
    { }

    template<bool _OtherConst>
      requires sentinel_for<_InnerSent<_Const>, _InnerIter<_OtherConst>>
    friend constexpr bool
    operator==(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    { return __x._M_inner == __y._M_inner; }

    template<bool _OtherConst>
      requires sized_sentinel_for<_InnerSent<_Const>, _InnerIter<_OtherConst>>
    friend constexpr range_difference_t<__detail::__maybe_const_t<_OtherConst, _InnerView>>
    operator-(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    { return __x._M_inner - __y._M_inner; }

    template<bool _OtherConst>
      requires sized_sentinel_for<_InnerSent<_Const>, _InnerIter<_OtherConst>>
    friend constexpr range_difference_t<__detail::__maybe_const_t<_OtherConst, _InnerView>>
    operator-(const _Sentinel& __x, const _Iterator<_OtherConst>& __y)
    { return __x._M_inner - __y._M_inner; }
  };

  namespace views
  {
    namespace __detail
    {
      template<size_t _Nm, typename _Range, typename _Fp>
	concept __can_adjacent_transform_view
	  = requires { adjacent_transform_view<std::views::all_t<_Range>, decay_t<_Fp>, _Nm>
		         (std::declval<_Range>(), std::declval<_Fp>()); };
    }

    template<size_t _Nm>
      struct _AdjacentTransform : std::views::__adaptor::_RangeAdaptor<_AdjacentTransform<_Nm>>
      {
	template<viewable_range _Range, typename _Fp>
	  requires (_Nm == 0) || __detail::__can_adjacent_transform_view<_Nm, _Range, _Fp>
	  constexpr auto
	  operator() [[nodiscard]] (_Range&& __r, _Fp&& __f) const
	  {
	    if constexpr (_Nm == 0)
	      return zip_transform(std::forward<_Fp>(__f));
	    else
	      return adjacent_transform_view<std::views::all_t<_Range>, decay_t<_Fp>, _Nm>
		(std::forward<_Range>(__r), std::forward<_Fp>(__f));
	  }

	using std::views::__adaptor::_RangeAdaptor<_AdjacentTransform>::operator();
	static constexpr int _S_arity = 2;
	static constexpr bool _S_has_simple_extra_args = true;
      };

    template<size_t _Nm>
      inline constexpr _AdjacentTransform<_Nm> adjacent_transform;

    inline constexpr auto pairwise_transform = adjacent_transform<2>;
  }

  template<forward_range _Vp,
	   indirect_binary_predicate<iterator_t<_Vp>, iterator_t<_Vp>> _Pred>
    requires view<_Vp> && is_object_v<_Pred>
  class chunk_by_view : public view_interface<chunk_by_view<_Vp, _Pred>>
  {
    _Vp _M_base = _Vp();
    __detail::__box<_Pred> _M_pred;
    __detail::_CachedPosition<_Vp> _M_cached_begin;

    static constexpr iterator_t<_Vp>
    _S_find_next(iterator_t<_Vp> __current, sentinel_t<_Vp> __end, const _Pred & __pred_)
    {
      auto __pred = [&]<typename _Tp, typename _Up>(_Tp&& __x, _Up&& __y) {
	return !bool(__pred_(std::forward<_Tp>(__x), std::forward<_Up>(__y)));
      };
      auto __it = ranges::adjacent_find(__current, __end, __pred);
      return ranges::next(__it, 1, __end);
    }

    static constexpr iterator_t<_Vp>
    _S_find_prev(iterator_t<_Vp> __begin, iterator_t<_Vp> __current, const _Pred & __pred_)
      requires bidirectional_range<_Vp>
    {
      auto __pred = [&]<typename _Tp, typename _Up>(_Tp&& __x, _Up&& __y) {
	return !bool(__pred_(std::forward<_Up>(__y), std::forward<_Tp>(__x)));
      };
      auto __rbegin = std::make_reverse_iterator(__current);
      auto __rend = std::make_reverse_iterator(__begin);
      __glibcxx_assert(__rbegin != __rend);
      auto __it = ranges::adjacent_find(__rbegin, __rend, __pred).base();
      return ranges::prev(__it, 1, __begin);
    }

    class _Iterator;

  public:
    chunk_by_view() requires (default_initializable<_Vp>
			      && default_initializable<_Pred>)
      = default;

    constexpr explicit
    chunk_by_view(_Vp __base, _Pred __pred)
    : _M_base(std::move(__base)), _M_pred(std::move(__pred))
    { }

    constexpr _Vp
    base() const & requires copy_constructible<_Vp>
    { return _M_base; }

    constexpr _Vp
    base() &&
    { return std::move(_M_base); }

    constexpr const _Pred&
    pred() const
    { return *_M_pred; }

    constexpr _Iterator
    begin()
    {
      __glibcxx_assert(_M_pred.has_value());
      iterator_t<_Vp> __it;
      if (_M_cached_begin._M_has_value())
	__it = _M_cached_begin._M_get(_M_base);
      else
	{
	  __it = _S_find_next(ranges::begin(_M_base), ranges::end(_M_base), *_M_pred);
	  _M_cached_begin._M_set(_M_base, __it);
	}
      return _Iterator(*this, ranges::begin(_M_base), __it);
    }

    constexpr auto
    end()
    {
      if constexpr (common_range<_Vp>)
	return _Iterator(*this, ranges::end(_M_base), ranges::end(_M_base));
      else
	return default_sentinel;
    }
  };

  template<typename _Range, typename _Pred>
    chunk_by_view(_Range&&, _Pred) -> chunk_by_view<std::views::all_t<_Range>, _Pred>;

  template<forward_range _Vp,
	   indirect_binary_predicate<iterator_t<_Vp>, iterator_t<_Vp>> _Pred>
    requires view<_Vp> && is_object_v<_Pred>
  class chunk_by_view<_Vp, _Pred>::_Iterator
  {
    using _Pred_access =
      conditional_t<__tidy_func<_Pred>, __detail::__box<_Pred>, chunk_by_view*>;

    [[no_unique_address]] _Pred_access _M_pred_access;
    iterator_t<_Vp> _M_begin = iterator_t<_Vp>();
    iterator_t<_Vp> _M_current = iterator_t<_Vp>();
    iterator_t<_Vp> _M_next = iterator_t<_Vp>();
    [[no_unique_address]] sentinel_t<_Vp> _M_end = sentinel_t<_Vp>();

    constexpr const _Pred& __pred() const
    {
      if constexpr (__tidy_func<_Pred>)
         return *_M_pred_access;
      else
        return *_M_pred_access->_M_pred;
    }

    constexpr
    _Iterator(chunk_by_view& __parent, iterator_t<_Vp> __current, iterator_t<_Vp> __next)
      : _M_pred_access([&]{
        if constexpr (__tidy_func<_Pred>)
          return __parent._M_pred;
        else
          return std::__addressof(__parent);
      }()),
      _M_begin(ranges::begin(__parent._M_base)),
      _M_current(__current), _M_next(__next),
      _M_end(ranges::end(__parent._M_base))
    { }

    static auto
    _S_iter_concept()
    {
      if constexpr (bidirectional_range<_Vp>)
	return bidirectional_iterator_tag{};
      else
	return forward_iterator_tag{};
    }

    friend chunk_by_view;

  public:
    using value_type = subrange<iterator_t<_Vp>>;
    using difference_type = range_difference_t<_Vp>;
    using iterator_category = input_iterator_tag;
    using iterator_concept = decltype(_S_iter_concept());

    _Iterator() = default;

    constexpr value_type
    operator*() const
    {
      __glibcxx_assert(_M_current != _M_next);
      return ranges::subrange(_M_current, _M_next);
    }

    constexpr _Iterator&
    operator++()
    {
      __glibcxx_assert(_M_current != _M_next);
      _M_current = _M_next;
      _M_next = _S_find_next(_M_current, _M_end, __pred());
      return *this;
    }

    constexpr _Iterator
    operator++(int)
    {
      auto __tmp = *this;
      ++*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator--() requires bidirectional_range<_Vp>
    {
      _M_next = _M_current;
      _M_current = _S_find_prev(_M_begin, _M_next, __pred());
      return *this;
    }

    constexpr _Iterator
    operator--(int) requires bidirectional_range<_Vp>
    {
      auto __tmp = *this;
      --*this;
      return __tmp;
    }

    friend constexpr bool
    operator==(const _Iterator& __x, const _Iterator& __y)
    { return __x._M_current == __y._M_current; }

    friend constexpr bool
    operator==(const _Iterator& __x, default_sentinel_t)
    { return __x._M_current == __x._M_next; }
  };

  namespace views
  {
    namespace __detail
    {
      template<typename _Range, typename _Pred>
	concept __can_chunk_by_view
	  = requires { chunk_by_view(std::declval<_Range>(), std::declval<_Pred>()); };
    }

    struct _ChunkBy : std::views::__adaptor::_RangeAdaptor<_ChunkBy>
    {
      template<viewable_range _Range, typename _Pred>
	requires __detail::__can_chunk_by_view<_Range, _Pred>
	constexpr auto
	operator() [[nodiscard]] (_Range&& __r, _Pred&& __pred) const
	{ return chunk_by_view(std::forward<_Range>(__r), std::forward<_Pred>(__pred)); }

      using std::views::__adaptor::_RangeAdaptor<_ChunkBy>::operator();
      static constexpr int _S_arity = 2;
      static constexpr bool _S_has_simple_extra_args = true;
    };

    inline constexpr _ChunkBy chunk_by;
  }

  template<input_range _Vp, forward_range _Pattern>
    requires view<_Vp> && view<_Pattern>
      && input_range<range_reference_t<_Vp>>
      && __detail::__compatible_joinable_ranges<range_reference_t<_Vp>, _Pattern>
  class join_with_view : public view_interface<join_with_view<_Vp, _Pattern>>
  {
    using _InnerRange = range_reference_t<_Vp>;

    _Vp _M_base = _Vp();
    __detail::__non_propagating_cache<remove_cv_t<_InnerRange>> _M_inner;
    _Pattern _M_pattern = _Pattern();

    template<bool _Const> using _Base = __detail::__maybe_const_t<_Const, _Vp>;
    template<bool _Const> using _InnerBase = range_reference_t<_Base<_Const>>;
    template<bool _Const> using _PatternBase = __detail::__maybe_const_t<_Const, _Pattern>;

    template<bool _Const> using _OuterIter = iterator_t<_Base<_Const>>;
    template<bool _Const> using _InnerIter = iterator_t<_InnerBase<_Const>>;
    template<bool _Const> using _PatternIter = iterator_t<_PatternBase<_Const>>;

    template<bool _Const>
      static constexpr bool _S_ref_is_glvalue = is_reference_v<_InnerBase<_Const>>;

    template<bool _Const>
    struct __iter_cat
    { };

    template<bool _Const>
      requires _S_ref_is_glvalue<_Const>
	&& forward_range<_Base<_Const>>
	&& forward_range<_InnerBase<_Const>>
    struct __iter_cat<_Const>
    {
      private:
	static auto
	_S_iter_cat()
	{
	  using _OuterIter = join_with_view::_OuterIter<_Const>;
	  using _InnerIter = join_with_view::_InnerIter<_Const>;
	  using _PatternIter = join_with_view::_PatternIter<_Const>;
	  using _OuterCat = typename iterator_traits<_OuterIter>::iterator_category;
	  using _InnerCat = typename iterator_traits<_InnerIter>::iterator_category;
	  using _PatternCat = typename iterator_traits<_PatternIter>::iterator_category;
	  if constexpr (!is_lvalue_reference_v<common_reference_t<iter_reference_t<_InnerIter>,
								  iter_reference_t<_PatternIter>>>)
	    return input_iterator_tag{};
	  else if constexpr (derived_from<_OuterCat, bidirectional_iterator_tag>
			     && derived_from<_InnerCat, bidirectional_iterator_tag>
			     && derived_from<_PatternCat, bidirectional_iterator_tag>
			     && common_range<_InnerBase<_Const>>
			     && common_range<_PatternBase<_Const>>)
	    return bidirectional_iterator_tag{};
	  else if constexpr (derived_from<_OuterCat, forward_iterator_tag>
			     && derived_from<_InnerCat, forward_iterator_tag>
			     && derived_from<_PatternCat, forward_iterator_tag>)
	    return forward_iterator_tag{};
	  else
	    return input_iterator_tag{};
	}
      public:
	using iterator_category = decltype(_S_iter_cat());
    };

    template<bool> struct _Iterator;
    template<bool> struct _Sentinel;

  public:
    join_with_view() requires (default_initializable<_Vp>
			       && default_initializable<_Pattern>)
      = default;

    constexpr
    join_with_view(_Vp __base, _Pattern __pattern)
    : _M_base(std::move(__base)), _M_pattern(std::move(__pattern))
    { }

    template<input_range _Range>
      requires constructible_from<_Vp, std::views::all_t<_Range>>
	&& constructible_from<_Pattern, single_view<range_value_t<_InnerRange>>>
    constexpr
    join_with_view(_Range&& __r, range_value_t<_InnerRange> __e)
    : _M_base(std::views::all(std::forward<_Range>(__r))),
      _M_pattern(std::views::single(std::move(__e)))
    { }

    constexpr _Vp
    base() const& requires copy_constructible<_Vp>
    { return _M_base; }

    constexpr _Vp
    base() &&
    { return std::move(_M_base); }

    constexpr auto
    begin()
    {
      constexpr bool __use_const = is_reference_v<_InnerRange>
	&& __detail::__simple_view<_Vp> && __detail::__simple_view<_Pattern>;
      return _Iterator<__use_const>{*this, ranges::begin(_M_base)};
    }

    constexpr auto
    begin() const
      requires input_range<const _Vp>
	&& forward_range<const _Pattern>
	&& is_reference_v<range_reference_t<const _Vp>>
    { return _Iterator<true>{*this, ranges::begin(_M_base)}; }

    constexpr auto
    end()
    {
      constexpr bool __use_const
	= __detail::__simple_view<_Vp> && __detail::__simple_view<_Pattern>;
      if constexpr (is_reference_v<_InnerRange>
		    && forward_range<_Vp> && common_range<_Vp>
		    && forward_range<_InnerRange> && common_range<_InnerRange>)
        return _Iterator<__use_const>{*this, ranges::end(_M_base)};
      else
        return _Sentinel<__use_const>{*this};
    }

    constexpr auto
    end() const
      requires input_range<const _Vp>
	&& forward_range<const _Pattern>
	&& is_reference_v<range_reference_t<const _Vp>>
    {
      using _InnerConstRange = range_reference_t<const _Vp>;
      if constexpr (forward_range<const _Vp>
		    && forward_range<_InnerConstRange>
		    && common_range<const _Vp>
		    && common_range<_InnerConstRange>)
        return _Iterator<true>{*this, ranges::end(_M_base)};
      else
        return _Sentinel<true>{*this};
    }
  };

  template<typename _Range, typename _Pattern>
    join_with_view(_Range&&, _Pattern&&)
      -> join_with_view<std::views::all_t<_Range>, std::views::all_t<_Pattern>>;

  template<input_range _Range>
    join_with_view(_Range&&, range_value_t<range_reference_t<_Range>>)
      -> join_with_view<std::views::all_t<_Range>,
			single_view<range_value_t<range_reference_t<_Range>>>>;

  template<input_range _Vp, forward_range _Pattern>
    requires view<_Vp> && view<_Pattern>
      && input_range<range_reference_t<_Vp>>
      && __detail::__compatible_joinable_ranges<range_reference_t<_Vp>, _Pattern>
  template<bool _Const>
  class join_with_view<_Vp, _Pattern>::_Iterator : public __iter_cat<_Const>
  {
    using _Parent = __detail::__maybe_const_t<_Const, join_with_view>;
    using _Base = join_with_view::_Base<_Const>;
    using _InnerBase = join_with_view::_InnerBase<_Const>;
    using _PatternBase = join_with_view::_PatternBase<_Const>;

    using _OuterIter = join_with_view::_OuterIter<_Const>;
    using _InnerIter = join_with_view::_InnerIter<_Const>;
    using _PatternIter = join_with_view::_PatternIter<_Const>;

    static constexpr bool _S_ref_is_glvalue = join_with_view::_S_ref_is_glvalue<_Const>;

    static auto
    _S_iter_concept()
    {
      if constexpr (_S_ref_is_glvalue
		    && bidirectional_range<_Base>
		    && __detail::__bidirectional_common<_InnerBase>
		    && __detail::__bidirectional_common<_PatternBase>)
	return bidirectional_iterator_tag{};
      else if constexpr (_S_ref_is_glvalue
			 && forward_range<_Base>
			 && forward_range<_InnerBase>)
	return forward_iterator_tag{};
      else
	return input_iterator_tag{};
    }

    static constexpr bool _S_store_pattern =
        __tidy_view<_Pattern> &&
        derived_from<decltype(_S_iter_concept()), std::forward_iterator_tag>;

    using _Pattern_access = conditional_t<_S_store_pattern, _Pattern, _Parent*>;

    [[no_unique_address]] _Pattern_access _M_pattern_access = _Pattern_access();
    _OuterIter _M_outer_it = _OuterIter();
    variant<_PatternIter, _InnerIter> _M_inner_it;
    [[no_unique_address]] sentinel_t<_Base> _M_end = sentinel_t<_Base>();

    constexpr _Pattern& __pattern()
    {
      if constexpr (_S_store_pattern)
        return _M_pattern_access;
      else
        return _M_pattern_access->_M_pattern;
    }

    constexpr
    _Iterator(_Parent& __parent, iterator_t<_Base> __outer)
    : _M_pattern_access([&]{
        if constexpr (_S_store_pattern)
          return __parent._M_pattern;
        else
          return std::__addressof(__parent);
      }()),
      _M_outer_it(std::move(__outer)),
      _M_end(ranges::end(__parent._M_base))
    {
      if (_M_outer_it != _M_end)
	{
	  auto&& __inner = _M_update_inner(_M_outer_it);
	  _M_inner_it.template emplace<1>(ranges::begin(__inner));
	  _M_satisfy();
	}
    }

    constexpr auto&&
    _M_update_inner(const _OuterIter& __x)
    {
      if constexpr (_S_ref_is_glvalue)
	return *__x;
      else
	return _M_pattern_access->_M_inner._M_emplace_deref(__x);
    }

    constexpr auto&&
    _M_get_inner(const _OuterIter& __x)
    {
      if constexpr (_S_ref_is_glvalue)
	return *__x;
      else
	return *_M_pattern_access->_M_inner;
    }

    constexpr void
    _M_satisfy()
    {
      while (true)
	{
	  if (_M_inner_it.index() == 0)
	    {
	      if (std::get<0>(_M_inner_it) != ranges::end(__pattern()))
		break;

	      auto&& __inner = _M_update_inner(_M_outer_it);
	      _M_inner_it.template emplace<1>(ranges::begin(__inner));
	    }
	  else
	    {
	      auto&& __inner = _M_get_inner(_M_outer_it);
	      if (std::get<1>(_M_inner_it) != ranges::end(__inner))
		break;

	      if (++_M_outer_it == _M_end)
		{
		  if constexpr (_S_ref_is_glvalue)
		    _M_inner_it.template emplace<0>();
		  break;
		}

	      _M_inner_it.template emplace<0>(ranges::begin(__pattern()));
	    }
	}
    }

    friend join_with_view;

  public:
    using iterator_concept = decltype(_S_iter_concept());
    // iterator_category defined in join_with_view::__iter_cat
    using value_type = common_type_t<iter_value_t<_InnerIter>,
				     iter_value_t<_PatternIter>>;
    using difference_type = common_type_t<iter_difference_t<_OuterIter>,
					  iter_difference_t<_InnerIter>,
					  iter_difference_t<_PatternIter>>;

    _Iterator() requires default_initializable<_OuterIter> = default;

    constexpr
    _Iterator(_Iterator<!_Const> __i)
      requires _Const
	&& convertible_to<iterator_t<_Vp>, _OuterIter>
	&& convertible_to<iterator_t<_InnerRange>, _InnerIter>
	&& convertible_to<iterator_t<_Pattern>, _PatternIter>
    : _M_pattern_access(__i._M_pattern_access),
      _M_outer_it(std::move(__i._M_outer_it)),
      _M_end(__i._M_end)
    {
      if (__i._M_inner_it.index() == 0)
	_M_inner_it.template emplace<0>(std::get<0>(std::move(__i._M_inner_it)));
      else
	_M_inner_it.template emplace<1>(std::get<1>(std::move(__i._M_inner_it)));
    }

    constexpr common_reference_t<iter_reference_t<_InnerIter>,
			         iter_reference_t<_PatternIter>>
    operator*() const
    {
      if (_M_inner_it.index() == 0)
	return *std::get<0>(_M_inner_it);
      else
	return *std::get<1>(_M_inner_it);
    }

    constexpr _Iterator&
    operator++()
    {
      if (_M_inner_it.index() == 0)
	++std::get<0>(_M_inner_it);
      else
	++std::get<1>(_M_inner_it);
      _M_satisfy();
      return *this;
    }

    constexpr void
    operator++(int)
    { ++*this; }

    constexpr _Iterator
    operator++(int)
      requires _S_ref_is_glvalue
	&& forward_iterator<_OuterIter> && forward_iterator<_InnerIter>
    {
      _Iterator __tmp = *this;
      ++*this;
      return __tmp;
    }

    constexpr _Iterator&
    operator--()
      requires _S_ref_is_glvalue
	&& bidirectional_range<_Base>
	&& __detail::__bidirectional_common<_InnerBase>
	&& __detail::__bidirectional_common<_PatternBase>
    {
      if (_M_outer_it == _M_end)
	{
	  auto&& __inner = *--_M_outer_it;
	  _M_inner_it.template emplace<1>(ranges::end(__inner));
	}

      while (true)
	{
	  if (_M_inner_it.index() == 0)
	    {
	      auto& __it = std::get<0>(_M_inner_it);
	      if (__it == ranges::begin(__pattern()))
		{
		  auto&& __inner = *--_M_outer_it;
		  _M_inner_it.template emplace<1>(ranges::end(__inner));
		}
	      else
		break;
	    }
	  else
	    {
	      auto& __it = std::get<1>(_M_inner_it);
	      auto&& __inner = *_M_outer_it;
	      if (__it == ranges::begin(__inner))
		_M_inner_it.template emplace<0>(ranges::end(__pattern()));
	      else
		break;
	    }
	}

      if (_M_inner_it.index() == 0)
	--std::get<0>(_M_inner_it);
      else
	--std::get<1>(_M_inner_it);
      return *this;
    }

    constexpr _Iterator
    operator--(int)
      requires _S_ref_is_glvalue && bidirectional_range<_Base>
	&& __detail::__bidirectional_common<_InnerBase>
	&& __detail::__bidirectional_common<_PatternBase>
    {
      _Iterator __tmp = *this;
      --*this;
      return __tmp;
    }

    friend constexpr bool
    operator==(const _Iterator& __x, const _Iterator& __y)
      requires _S_ref_is_glvalue
	&& equality_comparable<_OuterIter> && equality_comparable<_InnerIter>
    { return __x._M_outer_it == __y._M_outer_it && __x._M_inner_it ==__y._M_inner_it; }

    friend constexpr common_reference_t<iter_rvalue_reference_t<_InnerIter>,
					iter_rvalue_reference_t<_PatternIter>>
    iter_move(const _Iterator& __x)
    {
      if (__x._M_inner_it.index() == 0)
	return ranges::iter_move(std::get<0>(__x._M_inner_it));
      else
	return ranges::iter_move(std::get<1>(__x._M_inner_it));
    }

    friend constexpr void
    iter_swap(const _Iterator& __x, const _Iterator& __y)
      requires indirectly_swappable<_InnerIter, _PatternIter>
    {
      if (__x._M_inner_it.index() == 0)
	{
	  if (__y._M_inner_it.index() == 0)
	    ranges::iter_swap(std::get<0>(__x._M_inner_it), std::get<0>(__y._M_inner_it));
	  else
	    ranges::iter_swap(std::get<0>(__x._M_inner_it), std::get<1>(__y._M_inner_it));
	}
      else
	{
	  if (__y._M_inner_it.index() == 0)
	    ranges::iter_swap(std::get<1>(__x._M_inner_it), std::get<0>(__y._M_inner_it));
	  else
	    ranges::iter_swap(std::get<1>(__x._M_inner_it), std::get<1>(__y._M_inner_it));
	}
    }
  };

  template<input_range _Vp, forward_range _Pattern>
    requires view<_Vp> && view<_Pattern>
      && input_range<range_reference_t<_Vp>>
      && __detail::__compatible_joinable_ranges<range_reference_t<_Vp>, _Pattern>
  template<bool _Const>
  class join_with_view<_Vp, _Pattern>::_Sentinel
  {
    using _Parent = __detail::__maybe_const_t<_Const, join_with_view>;
    using _Base = join_with_view::_Base<_Const>;

    sentinel_t<_Base> _M_end = sentinel_t<_Base>();

    constexpr explicit
    _Sentinel(_Parent& __parent)
    : _M_end(ranges::end(__parent._M_base))
    { }

    friend join_with_view;

  public:
    _Sentinel() = default;

    constexpr
    _Sentinel(_Sentinel<!_Const> __s)
      requires _Const && convertible_to<sentinel_t<_Vp>, sentinel_t<_Base>>
    : _M_end(std::move(__s._M_end))
    { }

    template<bool _OtherConst>
      requires sentinel_for<sentinel_t<_Base>,
			    iterator_t<__detail::__maybe_const_t<_OtherConst, _Vp>>>
    friend constexpr bool
    operator==(const _Iterator<_OtherConst>& __x, const _Sentinel& __y)
    { return __x._M_outer_it == __y._M_end; }
  };

  namespace views
  {
    namespace __detail
    {
      template<typename _Range, typename _Pattern>
	concept __can_join_with_view
	  = requires { join_with_view(std::declval<_Range>(), std::declval<_Pattern>()); };
    } // namespace __detail

    struct _JoinWith : std::views::__adaptor::_RangeAdaptor<_JoinWith>
    {
      template<viewable_range _Range, typename _Pattern>
	requires __detail::__can_join_with_view<_Range, _Pattern>
	constexpr auto
	operator() [[nodiscard]] (_Range&& __r, _Pattern&& __f) const
	{
	  return join_with_view(std::forward<_Range>(__r), std::forward<_Pattern>(__f));
	}

      using _RangeAdaptor<_JoinWith>::operator();
      static constexpr int _S_arity = 2;
      template<typename _Pattern>
	static constexpr bool _S_has_simple_extra_args
	  = is_scalar_v<_Pattern> || (view<_Pattern>
				      && copy_constructible<_Pattern>);
        //_LazySplit::_S_has_simple_extra_args<_Pattern>;
    };

    inline constexpr _JoinWith join_with;
  } // namespace views

  template<input_range _Vp,
	   indirect_unary_predicate<iterator_t<_Vp>> _Pred>
    requires view<_Vp> && is_object_v<_Pred>
    class filter_view : public view_interface<filter_view<_Vp, _Pred>>
    {
    private:
      struct _Sentinel;

      struct _Iterator : __detail::__filter_view_iter_cat<_Vp>
      {
      private:
	static constexpr auto
	_S_iter_concept()
	{
	  if constexpr (bidirectional_range<_Vp>)
	    return bidirectional_iterator_tag{};
	  else if constexpr (forward_range<_Vp>)
	    return forward_iterator_tag{};
	  else
	    return input_iterator_tag{};
	}

	friend filter_view;

	using _Vp_iter = iterator_t<_Vp>;
	using _Vp_sent = sentinel_t<_Vp>;

	_Vp_iter _M_current = _Vp_iter();
        _Vp_sent _M_end = _Vp_sent();

        using _Pred_access =
          conditional_t<__tidy_func<_Pred>, __detail::__box<_Pred>, filter_view*>;

        [[no_unique_address]] _Pred_access _M_pred_access;

        constexpr const _Pred& __pred() const
        {
          if constexpr (__tidy_func<_Pred>)
             return *_M_pred_access;
          else
            return *_M_pred_access->_M_pred;
        }

      public:
	using iterator_concept = decltype(_S_iter_concept());
	// iterator_category defined in __filter_view_iter_cat
	using value_type = range_value_t<_Vp>;
	using difference_type = range_difference_t<_Vp>;

	_Iterator()
          requires default_initializable<_Vp_iter> && default_initializable<_Vp_sent>
        = default;

	constexpr
	_Iterator(filter_view* __parent, _Vp_iter __current)
	  : _M_current(std::move(__current)),
	    _M_end(ranges::end(__parent->_M_base)),
	    _M_pred_access([&]{
              if constexpr (__tidy_func<_Pred>)
                return __parent->_M_pred;
              else
                return __parent;
            }())
	{ }

	constexpr const _Vp_iter&
	base() const & noexcept
	{ return _M_current; }

	constexpr _Vp_iter
	base() &&
	{ return std::move(_M_current); }

	constexpr range_reference_t<_Vp>
	operator*() const
	{ return *_M_current; }

	constexpr _Vp_iter
	operator->() const
	  requires __detail::__has_arrow<_Vp_iter>
	    && copyable<_Vp_iter>
	{ return _M_current; }

	constexpr _Iterator&
	operator++()
	{
	  _M_current = ranges::find_if(std::move(++_M_current),
				       _M_end, std::ref(__pred()));
	  return *this;
	}

	constexpr void
	operator++(int)
	{ ++*this; }

	constexpr _Iterator
	operator++(int) requires forward_range<_Vp>
	{
	  auto __tmp = *this;
	  ++*this;
	  return __tmp;
	}

	constexpr _Iterator&
	operator--() requires bidirectional_range<_Vp>
	{
	  do
	    --_M_current;
	  while (!std::__invoke(__pred(), *_M_current));
	  return *this;
	}

	constexpr _Iterator
	operator--(int) requires bidirectional_range<_Vp>
	{
	  auto __tmp = *this;
	  --*this;
	  return __tmp;
	}

	friend constexpr bool
	operator==(const _Iterator& __x, const _Iterator& __y)
	  requires equality_comparable<_Vp_iter>
	{ return __x._M_current == __y._M_current; }

	friend constexpr range_rvalue_reference_t<_Vp>
	iter_move(const _Iterator& __i)
	  noexcept(noexcept(ranges::iter_move(__i._M_current)))
	{ return ranges::iter_move(__i._M_current); }

	friend constexpr void
	iter_swap(const _Iterator& __x, const _Iterator& __y)
	  noexcept(noexcept(ranges::iter_swap(__x._M_current, __y._M_current)))
	  requires indirectly_swappable<_Vp_iter>
	{ ranges::iter_swap(__x._M_current, __y._M_current); }
      };

      struct _Sentinel
      {
      private:
	sentinel_t<_Vp> _M_end = sentinel_t<_Vp>();

	constexpr bool
	__equal(const _Iterator& __i) const
	{ return __i._M_current == _M_end; }

      public:
	_Sentinel() = default;

	constexpr explicit
	_Sentinel(filter_view* __parent)
	  : _M_end(ranges::end(__parent->_M_base))
	{ }

	constexpr sentinel_t<_Vp>
	base() const
	{ return _M_end; }

	friend constexpr bool
	operator==(const _Iterator& __x, const _Sentinel& __y)
	{ return __y.__equal(__x); }
      };

      _Vp _M_base = _Vp();
      [[no_unique_address]] __detail::__box<_Pred> _M_pred;
      [[no_unique_address]] __detail::_CachedPosition<_Vp> _M_cached_begin;

    public:
      filter_view() requires (default_initializable<_Vp>
			      && default_initializable<_Pred>)
	= default;

      constexpr
      filter_view(_Vp __base, _Pred __pred)
	: _M_base(std::move(__base)), _M_pred(std::move(__pred))
      { }

      constexpr _Vp
      base() const& requires copy_constructible<_Vp>
      { return _M_base; }

      constexpr _Vp
      base() &&
      { return std::move(_M_base); }

      constexpr const _Pred&
      pred() const
      { return *_M_pred; }

      constexpr _Iterator
      begin()
      {
	if (_M_cached_begin._M_has_value())
	  return {this, _M_cached_begin._M_get(_M_base)};

	__glibcxx_assert(_M_pred.has_value());
	auto __it = ranges::find_if(ranges::begin(_M_base),
				    ranges::end(_M_base),
				    std::ref(*_M_pred));
	_M_cached_begin._M_set(_M_base, __it);
	return {this, std::move(__it)};
      }

      constexpr auto
      end()
      {
	if constexpr (common_range<_Vp>)
	  return _Iterator{this, ranges::end(_M_base)};
	else
	  return _Sentinel{this};
      }
    };

  template<typename _Range, typename _Pred>
    filter_view(_Range&&, _Pred) -> filter_view<std::views::all_t<_Range>, _Pred>;

  namespace views
  {
    namespace __detail
    {
      template<typename _Range, typename _Pred>
	concept __can_filter_view
	  = requires { filter_view(std::declval<_Range>(), std::declval<_Pred>()); };
    } // namespace __detail

    struct _Filter : std::views::__adaptor::_RangeAdaptor<_Filter>
    {
      template<viewable_range _Range, typename _Pred>
	requires __detail::__can_filter_view<_Range, _Pred>
	constexpr auto
	operator() [[nodiscard]] (_Range&& __r, _Pred&& __p) const
	{
	  return filter_view(std::forward<_Range>(__r), std::forward<_Pred>(__p));
	}

      using _RangeAdaptor<_Filter>::operator();
      static constexpr int _S_arity = 2;
      static constexpr bool _S_has_simple_extra_args = true;
    };

    inline constexpr _Filter filter;
  } // namespace views

  template<input_range _Vp, copy_constructible _Fp>
    requires view<_Vp> && is_object_v<_Fp>
      && regular_invocable<_Fp&, range_reference_t<_Vp>>
      && std::__detail::__can_reference<invoke_result_t<_Fp&,
							range_reference_t<_Vp>>>
    class transform_view : public view_interface<transform_view<_Vp, _Fp>>
    {
    private:
      template<bool _Const>
	using _Base = __detail::__maybe_const_t<_Const, _Vp>;

      template<bool _Const>
	struct __iter_cat
	{ };

      template<bool _Const>
	requires forward_range<_Base<_Const>>
	struct __iter_cat<_Const>
	{
	private:
	  static auto
	  _S_iter_cat()
	  {
	    using _Base = transform_view::_Base<_Const>;
	    using _Res = invoke_result_t<_Fp&, range_reference_t<_Base>>;
	    if constexpr (is_lvalue_reference_v<_Res>)
	      {
		using _Cat
		  = typename iterator_traits<iterator_t<_Base>>::iterator_category;
		if constexpr (derived_from<_Cat, contiguous_iterator_tag>)
		  return random_access_iterator_tag{};
		else
		  return _Cat{};
	      }
	    else
	      return input_iterator_tag{};
	  }
	public:
	  using iterator_category = decltype(_S_iter_cat());
	};

      template<bool _Const>
	struct _Sentinel;

      template<bool _Const>
	struct _Iterator : __iter_cat<_Const>
	{
	private:
	  using _Parent = __detail::__maybe_const_t<_Const, transform_view>;
	  using _Base = transform_view::_Base<_Const>;

	  static auto
	  _S_iter_concept()
	  {
	    if constexpr (random_access_range<_Base>)
	      return random_access_iterator_tag{};
	    else if constexpr (bidirectional_range<_Base>)
	      return bidirectional_iterator_tag{};
	    else if constexpr (forward_range<_Base>)
	      return forward_iterator_tag{};
	    else
	      return input_iterator_tag{};
	  }

	  using _Base_iter = iterator_t<_Base>;

	  _Base_iter _M_current = _Base_iter();

          using _F_access =
            conditional_t<__tidy_func<_Fp>, __detail::__box<_Fp>, _Parent*>;

          [[no_unique_address]] _F_access _M_f_access;

          constexpr const _Fp& __f() const
          {
              if constexpr (__tidy_func<_Fp>)
                  return *_M_f_access;
              else
                  return *_M_f_access->_M_fun;
          }

	  constexpr
	  _Iterator(const _F_access& __access, _Base_iter __current)
            requires __tidy_func<_Fp>
	    : _M_current(std::move(__current)),
	      _M_f_access(__access)
	  { }

	public:
	  using iterator_concept = decltype(_S_iter_concept());
	  // iterator_category defined in __transform_view_iter_cat
	  using value_type
	    = remove_cvref_t<invoke_result_t<_Fp&, range_reference_t<_Base>>>;
	  using difference_type = range_difference_t<_Base>;

	  _Iterator() requires default_initializable<_Base_iter> = default;

	  constexpr
	  _Iterator(_Parent* __parent, _Base_iter __current)
	    : _M_current(std::move(__current)),
	      _M_f_access([&]{
                if constexpr (__tidy_func<_Fp>)
                  return *__parent->_M_fun;
                else
                  return __parent;
              }())
	  { }

	  constexpr
	  _Iterator(_Iterator<!_Const> __i)
	    requires _Const
	      && convertible_to<iterator_t<_Vp>, _Base_iter>
	    : _M_current(std::move(__i._M_current)), _M_f_access(__i._M_f_access)
	  { }

	  constexpr const _Base_iter&
	  base() const & noexcept
	  { return _M_current; }

	  constexpr _Base_iter
	  base() &&
	  { return std::move(_M_current); }

	  constexpr decltype(auto)
	  operator*() const
            noexcept(noexcept(std::__invoke(__f(), *_M_current)))
	  { return std::__invoke(__f(), *_M_current); }

	  constexpr _Iterator&
	  operator++()
	  {
	    ++_M_current;
	    return *this;
	  }

	  constexpr void
	  operator++(int)
	  { ++_M_current; }

	  constexpr _Iterator
	  operator++(int) requires forward_range<_Base>
	  {
	    auto __tmp = *this;
	    ++*this;
	    return __tmp;
	  }

	  constexpr _Iterator&
	  operator--() requires bidirectional_range<_Base>
	  {
	    --_M_current;
	    return *this;
	  }

	  constexpr _Iterator
	  operator--(int) requires bidirectional_range<_Base>
	  {
	    auto __tmp = *this;
	    --*this;
	    return __tmp;
	  }

	  constexpr _Iterator&
	  operator+=(difference_type __n) requires random_access_range<_Base>
	  {
	    _M_current += __n;
	    return *this;
	  }

	  constexpr _Iterator&
	  operator-=(difference_type __n) requires random_access_range<_Base>
	  {
	    _M_current -= __n;
	    return *this;
	  }

	  constexpr decltype(auto)
	  operator[](difference_type __n) const
	    requires random_access_range<_Base>
	  { return std::__invoke(__f(), _M_current[__n]); }

	  friend constexpr bool
	  operator==(const _Iterator& __x, const _Iterator& __y)
	    requires equality_comparable<_Base_iter>
	  { return __x._M_current == __y._M_current; }

	  friend constexpr bool
	  operator<(const _Iterator& __x, const _Iterator& __y)
	    requires random_access_range<_Base>
	  { return __x._M_current < __y._M_current; }

	  friend constexpr bool
	  operator>(const _Iterator& __x, const _Iterator& __y)
	    requires random_access_range<_Base>
	  { return __y < __x; }

	  friend constexpr bool
	  operator<=(const _Iterator& __x, const _Iterator& __y)
	    requires random_access_range<_Base>
	  { return !(__y < __x); }

	  friend constexpr bool
	  operator>=(const _Iterator& __x, const _Iterator& __y)
	    requires random_access_range<_Base>
	  { return !(__x < __y); }

#ifdef __cpp_lib_three_way_comparison
	  friend constexpr auto
	  operator<=>(const _Iterator& __x, const _Iterator& __y)
	    requires random_access_range<_Base>
	      && three_way_comparable<_Base_iter>
	  { return __x._M_current <=> __y._M_current; }
#endif

	  friend constexpr _Iterator
	  operator+(_Iterator __i, difference_type __n)
	    requires random_access_range<_Base>
	  { return {__i._M_f_access, __i._M_current + __n}; }

	  friend constexpr _Iterator
	  operator+(difference_type __n, _Iterator __i)
	    requires random_access_range<_Base>
	  { return {__i._M_f_access, __i._M_current + __n}; }

	  friend constexpr _Iterator
	  operator-(_Iterator __i, difference_type __n)
	    requires random_access_range<_Base>
	  { return {__i._M_f_access, __i._M_current - __n}; }

	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 3483. transform_view::iterator's difference is overconstrained
	  friend constexpr difference_type
	  operator-(const _Iterator& __x, const _Iterator& __y)
	    requires sized_sentinel_for<iterator_t<_Base>, iterator_t<_Base>>
	  { return __x._M_current - __y._M_current; }

	  friend constexpr decltype(auto)
	  iter_move(const _Iterator& __i) noexcept(noexcept(*__i))
	  {
	    if constexpr (is_lvalue_reference_v<decltype(*__i)>)
	      return std::move(*__i);
	    else
	      return *__i;
	  }

	  friend _Iterator<!_Const>;
	  template<bool> friend struct _Sentinel;
	};

      template<bool _Const>
	struct _Sentinel
	{
	private:
	  using _Parent = __detail::__maybe_const_t<_Const, transform_view>;
	  using _Base = transform_view::_Base<_Const>;

	  template<bool _Const2>
	    constexpr auto
	    __distance_from(const _Iterator<_Const2>& __i) const
	    { return _M_end - __i._M_current; }

	  template<bool _Const2>
	    constexpr bool
	    __equal(const _Iterator<_Const2>& __i) const
	    { return __i._M_current == _M_end; }

	  sentinel_t<_Base> _M_end = sentinel_t<_Base>();

	public:
	  _Sentinel() = default;

	  constexpr explicit
	  _Sentinel(sentinel_t<_Base> __end)
	    : _M_end(__end)
	  { }

	  constexpr
	  _Sentinel(_Sentinel<!_Const> __i)
	    requires _Const
	      && convertible_to<sentinel_t<_Vp>, sentinel_t<_Base>>
	    : _M_end(std::move(__i._M_end))
	  { }

	  constexpr sentinel_t<_Base>
	  base() const
	  { return _M_end; }

	  template<bool _Const2>
	    requires sentinel_for<sentinel_t<_Base>,
		       iterator_t<__detail::__maybe_const_t<_Const2, _Vp>>>
	    friend constexpr bool
	    operator==(const _Iterator<_Const2>& __x, const _Sentinel& __y)
	    { return __y.__equal(__x); }

	  template<bool _Const2,
		   typename _Base2 = __detail::__maybe_const_t<_Const2, _Vp>>
	    requires sized_sentinel_for<sentinel_t<_Base>, iterator_t<_Base2>>
	    friend constexpr range_difference_t<_Base2>
	    operator-(const _Iterator<_Const2>& __x, const _Sentinel& __y)
	    { return -__y.__distance_from(__x); }

	  template<bool _Const2,
		   typename _Base2 = __detail::__maybe_const_t<_Const2, _Vp>>
	    requires sized_sentinel_for<sentinel_t<_Base>, iterator_t<_Base2>>
	    friend constexpr range_difference_t<_Base2>
	    operator-(const _Sentinel& __y, const _Iterator<_Const2>& __x)
	    { return __y.__distance_from(__x); }

	  friend _Sentinel<!_Const>;
	};

      _Vp _M_base = _Vp();
      [[no_unique_address]] __detail::__box<_Fp> _M_fun;

    public:
      transform_view() requires (default_initializable<_Vp>
				 && default_initializable<_Fp>)
	= default;

      constexpr
      transform_view(_Vp __base, _Fp __fun)
	: _M_base(std::move(__base)), _M_fun(std::move(__fun))
      { }

      constexpr _Vp
      base() const& requires copy_constructible<_Vp>
      { return _M_base ; }

      constexpr _Vp
      base() &&
      { return std::move(_M_base); }

      constexpr _Iterator<false>
      begin()
      { return _Iterator<false>{this, ranges::begin(_M_base)}; }

      constexpr _Iterator<true>
      begin() const
	requires range<const _Vp>
	  && regular_invocable<const _Fp&, range_reference_t<const _Vp>>
      { return _Iterator<true>{this, ranges::begin(_M_base)}; }

      constexpr _Sentinel<false>
      end()
      { return _Sentinel<false>{ranges::end(_M_base)}; }

      constexpr _Iterator<false>
      end() requires common_range<_Vp>
      { return _Iterator<false>{this, ranges::end(_M_base)}; }

      constexpr _Sentinel<true>
      end() const
	requires range<const _Vp>
	  && regular_invocable<const _Fp&, range_reference_t<const _Vp>>
      { return _Sentinel<true>{ranges::end(_M_base)}; }

      constexpr _Iterator<true>
      end() const
	requires common_range<const _Vp>
	  && regular_invocable<const _Fp&, range_reference_t<const _Vp>>
      { return _Iterator<true>{this, ranges::end(_M_base)}; }

      constexpr auto
      size() requires sized_range<_Vp>
      { return ranges::size(_M_base); }

      constexpr auto
      size() const requires sized_range<const _Vp>
      { return ranges::size(_M_base); }
    };

  template<typename _Range, typename _Fp>
    transform_view(_Range&&, _Fp) -> transform_view<std::views::all_t<_Range>, _Fp>;

  namespace views
  {
    namespace __detail
    {
      template<typename _Range, typename _Fp>
	concept __can_transform_view
	  = requires { transform_view(std::declval<_Range>(), std::declval<_Fp>()); };
    } // namespace __detail

    struct _Transform : std::views::__adaptor::_RangeAdaptor<_Transform>
    {
      template<viewable_range _Range, typename _Fp>
	requires __detail::__can_transform_view<_Range, _Fp>
	constexpr auto
	operator() [[nodiscard]] (_Range&& __r, _Fp&& __f) const
	{
	  return transform_view(std::forward<_Range>(__r), std::forward<_Fp>(__f));
	}

      using _RangeAdaptor<_Transform>::operator();
      static constexpr int _S_arity = 2;
      static constexpr bool _S_has_simple_extra_args = true;
    };

    inline constexpr _Transform transform;
  } // namespace views

  template<view _Vp, typename _Pred>
    requires input_range<_Vp> && is_object_v<_Pred>
      && indirect_unary_predicate<const _Pred, iterator_t<_Vp>>
    class take_while_view : public view_interface<take_while_view<_Vp, _Pred>>
    {
      template<bool _Const>
	struct _Sentinel
	{
	private:
	  using _Base = __detail::__maybe_const_t<_Const, _Vp>;

	  sentinel_t<_Base> _M_end = sentinel_t<_Base>();

          using _Pred_access =
            conditional_t<__tidy_func<_Pred>, __detail::__box<_Pred>, const _Pred*>;

          _Pred_access _M_pred;

	public:
	  _Sentinel() = default;

	  constexpr explicit
	  _Sentinel(sentinel_t<_Base> __end, const _Pred* __pred)
            : _M_end(__end), _M_pred([&]{
                if constexpr (__tidy_func<_Pred>)
                  return *__pred;
                else
                  return __pred;
              }())
	  { }

	  constexpr
	  _Sentinel(_Sentinel<!_Const> __s)
	    requires _Const && convertible_to<sentinel_t<_Vp>, sentinel_t<_Base>>
	    : _M_end(__s._M_end), _M_pred(__s._M_pred)
	  { }

	  constexpr sentinel_t<_Base>
	  base() const { return _M_end; }

	  friend constexpr bool
	  operator==(const iterator_t<_Base>& __x, const _Sentinel& __y)
	  { return __y._M_end == __x || !std::__invoke(*__y._M_pred, *__x); }

	  template<bool _OtherConst = !_Const,
		   typename _Base2 = __detail::__maybe_const_t<_OtherConst, _Vp>>
	    requires sentinel_for<sentinel_t<_Base>, iterator_t<_Base2>>
	  friend constexpr bool
	  operator==(const iterator_t<_Base2>& __x, const _Sentinel& __y)
	  { return __y._M_end == __x || !std::__invoke(*__y._M_pred, *__x); }

	  friend _Sentinel<!_Const>;
	};

      _Vp _M_base = _Vp();
      [[no_unique_address]] __detail::__box<_Pred> _M_pred;

    public:
      take_while_view() requires (default_initializable<_Vp>
				  && default_initializable<_Pred>)
	= default;

      constexpr
      take_while_view(_Vp __base, _Pred __pred)
	: _M_base(std::move(__base)), _M_pred(std::move(__pred))
      { }

      constexpr _Vp
      base() const& requires copy_constructible<_Vp>
      { return _M_base; }

      constexpr _Vp
      base() &&
      { return std::move(_M_base); }

      constexpr const _Pred&
      pred() const
      { return *_M_pred; }

      constexpr auto
      begin() requires (!__detail::__simple_view<_Vp>)
      { return ranges::begin(_M_base); }

      constexpr auto
      begin() const requires range<const _Vp>
	&& indirect_unary_predicate<const _Pred, iterator_t<const _Vp>>
      { return ranges::begin(_M_base); }

      constexpr auto
      end() requires (!__detail::__simple_view<_Vp>)
      { return _Sentinel<false>(ranges::end(_M_base),
				std::__addressof(*_M_pred)); }

      constexpr auto
      end() const requires range<const _Vp>
	&& indirect_unary_predicate<const _Pred, iterator_t<const _Vp>>
      { return _Sentinel<true>(ranges::end(_M_base),
			       std::__addressof(*_M_pred)); }
    };

  template<typename _Range, typename _Pred>
    take_while_view(_Range&&, _Pred)
      -> take_while_view<std::views::all_t<_Range>, _Pred>;

  namespace views
  {
    namespace __detail
    {
      template<typename _Range, typename _Pred>
	concept __can_take_while_view
	  = requires { take_while_view(std::declval<_Range>(), std::declval<_Pred>()); };
    } // namespace __detail

      struct _TakeWhile : std::views::__adaptor::_RangeAdaptor<_TakeWhile>
    {
      template<viewable_range _Range, typename _Pred>
	requires __detail::__can_take_while_view<_Range, _Pred>
	constexpr auto
	operator() [[nodiscard]] (_Range&& __r, _Pred&& __p) const
	{
	  return take_while_view(std::forward<_Range>(__r), std::forward<_Pred>(__p));
	}

      using _RangeAdaptor<_TakeWhile>::operator();
      static constexpr int _S_arity = 2;
      static constexpr bool _S_has_simple_extra_args = true;
    };

    inline constexpr _TakeWhile take_while;
  } // namespace views

  template<input_range _Vp, forward_range _Pattern>
    requires view<_Vp> && view<_Pattern>
      && indirectly_comparable<iterator_t<_Vp>, iterator_t<_Pattern>,
			       ranges::equal_to>
      && (forward_range<_Vp> || __detail::__tiny_range<_Pattern>)
    class lazy_split_view : public view_interface<lazy_split_view<_Vp, _Pattern>>
    {
    private:
      template<bool _Const>
	using _Base = __detail::__maybe_const_t<_Const, _Vp>;

      template<bool _Const>
	struct _InnerIter;

      template<bool _Const>
	struct _OuterIter
	  : __detail::__lazy_split_view_outer_iter_cat<_Base<_Const>>
	{
	private:
	  using _Parent = __detail::__maybe_const_t<_Const, lazy_split_view>;
	  using _Base = lazy_split_view::_Base<_Const>;

	  constexpr bool
	  __at_end() const
	  { return __current() == _M_end && !_M_trailing_empty; }

	  // [range.lazy.split.outer] p1
	  //  Many of the following specifications refer to the notional member
	  //  current of outer-iterator.  current is equivalent to current_ if
	  //  V models forward_range, and parent_->current_ otherwise.
	  constexpr auto&
	  __current() noexcept
	  {
	    if constexpr (forward_range<_Vp>)
	      return _M_current;
	    else
	      return *_M_pattern_access->_M_current;
	  }

	  constexpr auto&
	  __current() const noexcept
	  {
	    if constexpr (forward_range<_Vp>)
	      return _M_current;
	    else
	      return *_M_pattern_access->_M_current;
	  }

          using _Pattern_access =
            conditional_t<__tidy_view<_Pattern> && forward_range<_Base>, _Pattern, _Parent*>;

          [[no_unique_address]] _Pattern_access _M_pattern_access = _Pattern_access();
          [[no_unique_address]] sentinel_t<_Base> _M_end = sentinel_t<_Base>();

	  [[no_unique_address]]
	    __detail::__maybe_present_t<forward_range<_Vp>,
					iterator_t<_Base>> _M_current;
	  bool _M_trailing_empty = false;

          constexpr const _Pattern& __pattern() const
          {
            if constexpr (__tidy_view<_Pattern> && forward_range<_Base>)
              return _M_pattern_access;
            else
              return _M_pattern_access->_M_pattern;
          }

	public:
	  using iterator_concept = __conditional_t<forward_range<_Base>,
						   forward_iterator_tag,
						   input_iterator_tag>;
	  // iterator_category defined in __lazy_split_view_outer_iter_cat
	  using difference_type = range_difference_t<_Base>;

	  struct value_type : view_interface<value_type>
	  {
	  private:
	    _OuterIter _M_i = _OuterIter();

	  public:
	    value_type() = default;

	    constexpr explicit
	    value_type(_OuterIter __i)
	      : _M_i(std::move(__i))
	    { }

	    constexpr _InnerIter<_Const>
	    begin() const
	    { return _InnerIter<_Const>{_M_i}; }

	    constexpr default_sentinel_t
	    end() const noexcept
	    { return default_sentinel; }
	  };

          _OuterIter() requires default_initializable<sentinel_t<_Base>> = default;

	  constexpr explicit
	  _OuterIter(_Parent* __parent) requires (!forward_range<_Base>)
            : _M_pattern_access(__parent),
              _M_end(ranges::end(__parent->_M_base))
	  { }

	  constexpr
	  _OuterIter(_Parent* __parent, iterator_t<_Base> __current)
	    requires forward_range<_Base>
	    : _M_pattern_access([&]{
                if constexpr (__tidy_view<_Pattern>)
                  return __parent->_M_pattern;
                else
                  return __parent;
              }()),
              _M_end(ranges::end(__parent->_M_base)),
	      _M_current(std::move(__current))
	  { }

	  constexpr
	  _OuterIter(_OuterIter<!_Const> __i)
	    requires _Const
	      && convertible_to<iterator_t<_Vp>, iterator_t<_Base>>
            : _M_pattern_access(__i._M_pattern_access), _M_end(__i._M_end),
              _M_current(std::move(__i._M_current)),
	      _M_trailing_empty(__i._M_trailing_empty)
	  { }

	  constexpr value_type
	  operator*() const
	  { return value_type{*this}; }

	  constexpr _OuterIter&
	  operator++()
	  {
	    // _GLIBCXX_RESOLVE_LIB_DEFECTS
	    // 3505. lazy_split_view::outer-iterator::operator++ misspecified
	    if (__current() == _M_end)
	      {
		_M_trailing_empty = false;
		return *this;
	      }
	    const auto [__pbegin, __pend] = subrange{__pattern()};
	    if (__pbegin == __pend)
	      ++__current();
	    else if constexpr (__detail::__tiny_range<_Pattern>)
	      {
		__current() = ranges::find(std::move(__current()), _M_end,
					   *__pbegin);
		if (__current() != _M_end)
		  {
		    ++__current();
		    if (__current() == _M_end)
		      _M_trailing_empty = true;
		  }
	      }
	    else
	      do
		{
		  auto [__b, __p]
		    = ranges::mismatch(__current(), _M_end, __pbegin, __pend);
		  if (__p == __pend)
		    {
		      __current() = __b;
		      if (__current() == _M_end)
			_M_trailing_empty = true;
		      break;
		    }
		} while (++__current() != _M_end);
	    return *this;
	  }

	  constexpr decltype(auto)
	  operator++(int)
	  {
	    if constexpr (forward_range<_Base>)
	      {
		auto __tmp = *this;
		++*this;
		return __tmp;
	      }
	    else
	      ++*this;
	  }

	  friend constexpr bool
	  operator==(const _OuterIter& __x, const _OuterIter& __y)
	    requires forward_range<_Base>
	  {
	    return __x._M_current == __y._M_current
	      && __x._M_trailing_empty == __y._M_trailing_empty;
	  }

	  friend constexpr bool
	  operator==(const _OuterIter& __x, default_sentinel_t)
	  { return __x.__at_end(); };

	  friend _OuterIter<!_Const>;
	  friend _InnerIter<_Const>;
	};

      template<bool _Const>
	struct _InnerIter
	  : __detail::__lazy_split_view_inner_iter_cat<_Base<_Const>>
	{
	private:
	  using _Base = lazy_split_view::_Base<_Const>;

	  constexpr bool
	  __at_end() const
	  {
	    auto [__pcur, __pend] = subrange{_M_i.__pattern()};
	    auto __end = _M_i._M_end;
	    if constexpr (__detail::__tiny_range<_Pattern>)
	      {
		const auto& __cur = _M_i_current();
		if (__cur == __end)
		  return true;
		if (__pcur == __pend)
		  return _M_incremented;
		return *__cur == *__pcur;
	      }
	    else
	      {
		auto __cur = _M_i_current();
		if (__cur == __end)
		  return true;
		if (__pcur == __pend)
		  return _M_incremented;
		do
		  {
		    if (*__cur != *__pcur)
		      return false;
		    if (++__pcur == __pend)
		      return true;
		  } while (++__cur != __end);
		return false;
	      }
	  }

	  constexpr auto&
	  _M_i_current() noexcept
	  { return _M_i.__current(); }

	  constexpr auto&
	  _M_i_current() const noexcept
	  { return _M_i.__current(); }

	  _OuterIter<_Const> _M_i = _OuterIter<_Const>();
	  bool _M_incremented = false;

	public:
	  using iterator_concept
	    = typename _OuterIter<_Const>::iterator_concept;
	  // iterator_category defined in __lazy_split_view_inner_iter_cat
	  using value_type = range_value_t<_Base>;
	  using difference_type = range_difference_t<_Base>;

	  _InnerIter() = default;

	  constexpr explicit
	  _InnerIter(_OuterIter<_Const> __i)
	    : _M_i(std::move(__i))
	  { }

	  constexpr const iterator_t<_Base>&
	  base() const& noexcept
	  { return _M_i_current(); }

	  constexpr iterator_t<_Base>
	  base() && requires forward_range<_Vp>
	  { return std::move(_M_i_current()); }

	  constexpr decltype(auto)
	  operator*() const
	  { return *_M_i_current(); }

	  constexpr _InnerIter&
	  operator++()
	  {
	    _M_incremented = true;
	    if constexpr (!forward_range<_Base>)
	      if constexpr (_Pattern::size() == 0)
		return *this;
	    ++_M_i_current();
	    return *this;
	  }

	  constexpr decltype(auto)
	  operator++(int)
	  {
	    if constexpr (forward_range<_Base>)
	      {
		auto __tmp = *this;
		++*this;
		return __tmp;
	      }
	    else
	      ++*this;
	  }

	  friend constexpr bool
	  operator==(const _InnerIter& __x, const _InnerIter& __y)
	    requires forward_range<_Base>
	  { return __x._M_i == __y._M_i; }

	  friend constexpr bool
	  operator==(const _InnerIter& __x, default_sentinel_t)
	  { return __x.__at_end(); }

	  friend constexpr decltype(auto)
	  iter_move(const _InnerIter& __i)
	    noexcept(noexcept(ranges::iter_move(__i._M_i_current())))
	  { return ranges::iter_move(__i._M_i_current()); }

	  friend constexpr void
	  iter_swap(const _InnerIter& __x, const _InnerIter& __y)
	    noexcept(noexcept(ranges::iter_swap(__x._M_i_current(),
						__y._M_i_current())))
	    requires indirectly_swappable<iterator_t<_Base>>
	  { ranges::iter_swap(__x._M_i_current(), __y._M_i_current()); }
	};

      _Vp _M_base = _Vp();
      _Pattern _M_pattern = _Pattern();
      [[no_unique_address]]
	__detail::__maybe_present_t<!forward_range<_Vp>,
	  __detail::__non_propagating_cache<iterator_t<_Vp>>> _M_current;


    public:
      lazy_split_view() requires (default_initializable<_Vp>
				  && default_initializable<_Pattern>)
	= default;

      constexpr
      lazy_split_view(_Vp __base, _Pattern __pattern)
	: _M_base(std::move(__base)), _M_pattern(std::move(__pattern))
      { }

      template<input_range _Range>
	requires constructible_from<_Vp, std::views::all_t<_Range>>
	  && constructible_from<_Pattern, single_view<range_value_t<_Range>>>
	constexpr
	lazy_split_view(_Range&& __r, range_value_t<_Range> __e)
	  : _M_base(std::views::all(std::forward<_Range>(__r))),
	    _M_pattern(std::views::single(std::move(__e)))
	{ }

      constexpr _Vp
      base() const& requires copy_constructible<_Vp>
      { return _M_base; }

      constexpr _Vp
      base() &&
      { return std::move(_M_base); }

      constexpr auto
      begin()
      {
	if constexpr (forward_range<_Vp>)
	  {
	    constexpr bool __simple
	      = __detail::__simple_view<_Vp> && __detail::__simple_view<_Pattern>;
	    return _OuterIter<__simple>{this, ranges::begin(_M_base)};
	  }
	else
	  {
	    _M_current = ranges::begin(_M_base);
	    return _OuterIter<false>{this};
	  }
      }

      constexpr auto
      begin() const requires forward_range<_Vp> && forward_range<const _Vp>
      {
	return _OuterIter<true>{this, ranges::begin(_M_base)};
      }

      constexpr auto
      end() requires forward_range<_Vp> && common_range<_Vp>
      {
	constexpr bool __simple
	  = __detail::__simple_view<_Vp> && __detail::__simple_view<_Pattern>;
	return _OuterIter<__simple>{this, ranges::end(_M_base)};
      }

      constexpr auto
      end() const
      {
	if constexpr (forward_range<_Vp>
		      && forward_range<const _Vp>
		      && common_range<const _Vp>)
	  return _OuterIter<true>{this, ranges::end(_M_base)};
	else
	  return default_sentinel;
      }
    };

  template<typename _Range, typename _Pattern>
    lazy_split_view(_Range&&, _Pattern&&)
      -> lazy_split_view<std::views::all_t<_Range>, std::views::all_t<_Pattern>>;

  template<input_range _Range>
    lazy_split_view(_Range&&, range_value_t<_Range>)
      -> lazy_split_view<std::views::all_t<_Range>, single_view<range_value_t<_Range>>>;

  namespace views
  {
    namespace __detail
    {
      template<typename _Range, typename _Pattern>
	concept __can_lazy_split_view
	  = requires { lazy_split_view(std::declval<_Range>(), std::declval<_Pattern>()); };
    } // namespace __detail

    struct _LazySplit : std::views::__adaptor::_RangeAdaptor<_LazySplit>
    {
      template<viewable_range _Range, typename _Pattern>
	requires __detail::__can_lazy_split_view<_Range, _Pattern>
	constexpr auto
	operator() [[nodiscard]] (_Range&& __r, _Pattern&& __f) const
	{
	  return lazy_split_view(std::forward<_Range>(__r), std::forward<_Pattern>(__f));
	}

      using _RangeAdaptor<_LazySplit>::operator();
      static constexpr int _S_arity = 2;
      // The pattern argument of views::lazy_split is not always simple -- it can be
      // a non-view range, the value category of which affects whether the call
      // is well-formed.  But a scalar or a view pattern argument is surely
      // simple.
      template<typename _Pattern>
	static constexpr bool _S_has_simple_extra_args
	  = is_scalar_v<_Pattern> || (view<_Pattern>
				      && copy_constructible<_Pattern>);
    };

    inline constexpr _LazySplit lazy_split;
  } // namespace views

  template<forward_range _Vp, forward_range _Pattern>
    requires view<_Vp> && view<_Pattern>
      && indirectly_comparable<iterator_t<_Vp>, iterator_t<_Pattern>,
			       ranges::equal_to>
  class split_view : public view_interface<split_view<_Vp, _Pattern>>
  {
  private:
    _Vp _M_base = _Vp();
    _Pattern _M_pattern = _Pattern();
    __detail::__non_propagating_cache<subrange<iterator_t<_Vp>>> _M_cached_begin;

    struct _Iterator;
    struct _Sentinel;

  public:
    split_view() requires (default_initializable<_Vp>
			   && default_initializable<_Pattern>)
      = default;

    constexpr
    split_view(_Vp __base, _Pattern __pattern)
      : _M_base(std::move(__base)), _M_pattern(std::move(__pattern))
    { }

    template<forward_range _Range>
      requires constructible_from<_Vp, std::views::all_t<_Range>>
	&& constructible_from<_Pattern, single_view<range_value_t<_Range>>>
    constexpr
    split_view(_Range&& __r, range_value_t<_Range> __e)
      : _M_base(std::views::all(std::forward<_Range>(__r))),
	_M_pattern(std::views::single(std::move(__e)))
    { }

    constexpr _Vp
    base() const& requires copy_constructible<_Vp>
    { return _M_base; }

    constexpr _Vp
    base() &&
    { return std::move(_M_base); }

    constexpr _Iterator
    begin()
    {
      if (!_M_cached_begin)
        _M_cached_begin = _S_find_next(
          ranges::begin(_M_base), ranges::end(_M_base), _M_pattern);
      return {this, ranges::begin(_M_base), *_M_cached_begin};
    }

    constexpr auto
    end()
    {
      if constexpr (common_range<_Vp>)
	return _Iterator{this, ranges::end(_M_base), {}};
      else
	return _Sentinel{this};
    }

    static constexpr subrange<iterator_t<_Vp>>
    _S_find_next(iterator_t<_Vp> __it, iterator_t<_Vp> __end, const _Pattern& __pattern)
    {
      auto [__b, __e] = ranges::search(subrange(__it, __end), __pattern);
      if (__b != __end && ranges::empty(__pattern))
	{
	  ++__b;
	  ++__e;
	}
      return {__b, __e};
    }

  private:
    struct _Iterator
    {
    private:
      using _Pattern_access =
        conditional_t<__tidy_view<_Pattern>, _Pattern, split_view*>;

      [[no_unique_address]] _Pattern_access _M_pattern_access = _Pattern_access();
      iterator_t<_Vp> _M_cur = iterator_t<_Vp>();
      subrange<iterator_t<_Vp>> _M_next = subrange<iterator_t<_Vp>>();
      [[no_unique_address]] sentinel_t<_Vp> _M_end = sentinel_t<_Vp>();
      bool _M_trailing_empty = false;

      friend struct _Sentinel;

      constexpr const _Pattern& __pattern() const
      {
        if constexpr (__tidy_view<_Pattern>)
          return _M_pattern_access;
        else
          return _M_pattern_access->_M_pattern;
      }

    public:
      using iterator_concept = forward_iterator_tag;
      using iterator_category = input_iterator_tag;
      using value_type = subrange<iterator_t<_Vp>>;
      using difference_type = range_difference_t<_Vp>;

      _Iterator() = default;

      constexpr
      _Iterator(split_view* __parent,
		iterator_t<_Vp> __current,
		subrange<iterator_t<_Vp>> __next)
        : _M_pattern_access([&]{
            if constexpr (__tidy_view<_Pattern>)
              return __parent->_M_pattern;
            else
              return __parent;
          }()),
	  _M_cur(std::move(__current)),
	  _M_next(std::move(__next)),
	  _M_end(ranges::end(__parent->_M_base))
      { }

      constexpr iterator_t<_Vp>
      base() const
      { return _M_cur; }

      constexpr value_type
      operator*() const
      { return {_M_cur, _M_next.begin()}; }

      constexpr _Iterator&
      operator++()
      {
	_M_cur = _M_next.begin();
	if (_M_cur != _M_end)
	  {
	    _M_cur = _M_next.end();
	    if (_M_cur == _M_end)
	      {
		_M_trailing_empty = true;
		_M_next = {_M_cur, _M_cur};
	      }
	    else
              _M_next = split_view::_S_find_next(_M_cur, _M_end, __pattern());
	  }
	else
	  _M_trailing_empty = false;
	return *this;
      }

      constexpr _Iterator
      operator++(int)
      {
	auto __tmp = *this;
	++*this;
	return __tmp;
      }

      friend constexpr bool
      operator==(const _Iterator& __x, const _Iterator& __y)
      {
	return __x._M_cur == __y._M_cur
	  && __x._M_trailing_empty == __y._M_trailing_empty;
      }
    };

    struct _Sentinel
    {
    private:
      sentinel_t<_Vp> _M_end = sentinel_t<_Vp>();

      constexpr bool
      _M_equal(const _Iterator& __x) const
      { return __x._M_cur == _M_end && !__x._M_trailing_empty; }

    public:
      _Sentinel() = default;

      constexpr explicit
      _Sentinel(split_view* __parent)
	: _M_end(ranges::end(__parent->_M_base))
      { }

      friend constexpr bool
      operator==(const _Iterator& __x, const _Sentinel& __y)
      { return __y._M_equal(__x); }
    };
  };

  template<typename _Range, typename _Pattern>
    split_view(_Range&&, _Pattern&&)
      -> split_view<std::views::all_t<_Range>, std::views::all_t<_Pattern>>;

  template<forward_range _Range>
    split_view(_Range&&, range_value_t<_Range>)
      -> split_view<std::views::all_t<_Range>, single_view<range_value_t<_Range>>>;

  namespace views
  {
    namespace __detail
    {
      template<typename _Range, typename _Pattern>
	concept __can_split_view
	  = requires { split_view(std::declval<_Range>(), std::declval<_Pattern>()); };
    } // namespace __detail

    struct _Split : std::views::__adaptor::_RangeAdaptor<_Split>
    {
      template<viewable_range _Range, typename _Pattern>
	requires __detail::__can_split_view<_Range, _Pattern>
	constexpr auto
	operator() [[nodiscard]] (_Range&& __r, _Pattern&& __f) const
	{
	  return split_view(std::forward<_Range>(__r), std::forward<_Pattern>(__f));
	}

      using _RangeAdaptor<_Split>::operator();
      static constexpr int _S_arity = 2;
      template<typename _Pattern>
	static constexpr bool _S_has_simple_extra_args
	  = _LazySplit::_S_has_simple_extra_args<_Pattern>;
    };

    inline constexpr _Split split;
  } // namespace views
}

template<typename... _Views>
constexpr bool
    std::ranges::enable_borrowed_range<std::ranges::z::zip_view<_Views...>> =
        (std::ranges::enable_borrowed_range<_Views> && ...);

template<typename _Fp, typename... _Views>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::zip_transform_view<_Fp, _Views...>> =
    (std::ranges::enable_borrowed_range<_Views> && ...) && std::ranges::z::__tidy_func<_Fp>;

template<typename _Vp, size_t _Nm>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::adjacent_view<_Vp, _Nm>> = std::ranges::enable_borrowed_range<_Vp>;

template<typename _Vp, typename _Fp, size_t _Nm>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::adjacent_transform_view<_Vp, _Fp, _Nm>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Fp>;

template<typename _Vp, typename _Pred>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::chunk_by_view<_Vp, _Pred>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Pred>;

template<typename _Vp, typename _Pattern>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::join_with_view<_Vp, _Pattern>> =
    std::ranges::enable_borrowed_range<_Vp> &&
    std::ranges::forward_range<std::ranges::z::join_with_view<_Vp, _Pattern>> &&
    (std::ranges::enable_borrowed_range<_Pattern> || std::ranges::z::__tidy_view<_Pattern>);

template<typename _Vp, typename _Pred>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::filter_view<_Vp, _Pred>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Pred>;

template<typename _Vp, typename _Fp>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::transform_view<_Vp, _Fp>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Fp>;

template<typename _Vp, typename _Pred>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::take_while_view<_Vp, _Pred>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Pred>;

template<typename _Vp, typename _Pattern>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::lazy_split_view<_Vp, _Pattern>> =
    std::ranges::enable_borrowed_range<_Vp> &&
    std::ranges::forward_range<std::ranges::z::lazy_split_view<_Vp, _Pattern>> &&
    (std::ranges::enable_borrowed_range<_Pattern> || std::ranges::z::__tidy_view<_Pattern>);

template<typename _Vp, typename _Pattern>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::split_view<_Vp, _Pattern>> =
    std::ranges::enable_borrowed_range<_Vp> &&
    (std::ranges::enable_borrowed_range<_Pattern> || std::ranges::z::__tidy_view<_Pattern>);

template<typename _Vp>
constexpr bool std::ranges::enable_borrowed_range<std::ranges::join_view<_Vp>> =
    std::ranges::enable_borrowed_range<_Vp> &&
    std::ranges::forward_range<std::ranges::join_view<_Vp>>;


template<typename F>
struct fat_callable
{
    template<typename... Args>
    auto operator()(Args &&... args) const
    {
        return f((Args &&) args...);
    }
    F f;
    std::array<int, 256> extra_bytes;
};

// strike ub stuff
// talk about sizes (don't forget about sizes when there are lots of adaptors
// talk about timings and code gen
// split borrowed based on pattern trivial_copyabilty OR borrowedness


int main()
{
    std::vector<int> vec = {1, 2, 3, 4, 5, 6};
    std::vector<int> vec2 = {2, 3, 4, 5, 6, 1};

    {
        std::cout << "=== zip_transform ===\n\n";

        auto minus = [](auto x, auto y) { return x - y; };

        auto old_view = std::views::zip_transform(minus, vec, vec2);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = std::ranges::z::views::zip_transform(minus, vec, vec2);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = std::ranges::z::views::zip_transform(
            fat_callable(minus), vec, vec2);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== chunk_by ===\n\n";

        auto lt = [](auto x, auto y) { return x < y; };

        auto old_view = vec2 | std::views::chunk_by(lt);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto chunk : old_view) {
            for (auto x : chunk) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view = vec2 | std::ranges::z::views::chunk_by(lt);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto chunk : new_view) {
            for (auto x : chunk) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec2 | std::ranges::z::views::chunk_by(fat_callable(lt));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto chunk : fat_new_view) {
            for (auto x : chunk) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== join_with ===\n\n";

        using subrange_t = std::ranges::subrange<std::vector<int>::iterator>;
        std::vector<subrange_t> subranges = {
            {vec.begin(), vec.begin() + 2},
            {vec.begin() + 3, vec.begin() + 4},
            {vec.begin() + 4, vec.begin() + 6}};

        std::vector<int> pattern = {8, 9};

#if 0
        auto ints = std::istringstream{"1 2 3 4 5 6"};
        auto old_view = std::views::istream<int>(ints) | std::ranges::z::views::join_with(pattern);
#else
        auto old_view = subranges | std::views::join_with(pattern);
#endif
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = subranges | std::ranges::z::views::join_with(std::ranges::subrange(pattern));
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view_single = subranges | std::ranges::z::views::join_with(99);
        std::cout << "borrowed<new_view_single> = " << std::ranges::borrowed_range<decltype(new_view_single)> << "\n";
        std::cout << "sizeof(new_view_single.begin()) = " << sizeof(new_view_single.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view_single)>);
        for (auto x : new_view_single) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

#if 0
        auto ints = std::istringstream{"1 2 3 4 5 6"};
        auto input_view_single = std::views::istream<int>(ints) | std::ranges::z::views::join_with(99);
        std::cout << "borrowed<input_view_single> = " << std::ranges::borrowed_range<decltype(input_view_single)> << "\n";
        std::cout << "sizeof(input_view_single.begin()) = " << sizeof(input_view_single.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(input_view_single)>);
        for (auto x : input_view_single) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
#endif

        auto fat_new_view = subranges | std::ranges::z::views::join_with(pattern);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== adjacent_transform ===\n\n";

        auto minus = [](auto x, auto y) { return x - y; };

        auto old_view = vec | std::views::adjacent_transform<2>(minus);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view =
            vec | std::ranges::z::views::adjacent_transform<2>(minus);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = vec | std::ranges::z::views::adjacent_transform<2>(
                                      fat_callable(minus));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== filter ===\n\n";

        auto even = [](auto x) { return x % 2 == 0; };

        auto old_view = vec | std::views::filter(even);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::filter(even);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::filter(fat_callable(even));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== transform ===\n\n";

        auto negate = [](auto x) { return -x; };

        auto old_view = vec | std::views::transform(negate);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::transform(negate);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::transform(fat_callable(negate));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== take_while ===\n\n";

        auto lt_four = [](auto x) { return x < 4; };

        auto old_view = vec | std::views::take_while(lt_four);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::take_while(lt_four);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::take_while(fat_callable(lt_four));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== lazy_split_view ===\n\n";

        std::vector<int> pattern({3});

        auto old_view = vec | std::views::lazy_split(pattern);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        for (auto subrng : old_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view =
            vec | std::ranges::z::views::lazy_split(std::ranges::subrange(pattern));
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto subrng : new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view_single = vec | std::ranges::z::views::lazy_split(3);
        std::cout << "borrowed<new_view_single> = " << std::ranges::borrowed_range<decltype(new_view_single)> << "\n";
        std::cout << "sizeof(new_view_single.begin()) = "
                  << sizeof(new_view_single.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view_single)>);
        for (auto subrng : new_view_single) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto ints = std::istringstream{"1 2 3 4 5 6"};
        auto input_view_single = std::views::istream<int>(ints) |
                                 std::ranges::z::views::lazy_split(3);
        std::cout << "borrowed<input_view_single> = " << std::ranges::borrowed_range<decltype(input_view_single)> << "\n";
        std::cout << "sizeof(input_view_single.begin()) = "
                  << sizeof(input_view_single.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(input_view_single)>);
        for (auto subrng : input_view_single) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = vec | std::ranges::z::views::lazy_split(pattern);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto subrng : fat_new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== split_view ===\n\n";

        std::vector<int> pattern({3});

        auto old_view = vec | std::views::split(pattern);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        for (auto subrng : old_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view =
            vec | std::ranges::z::views::split(std::ranges::subrange(pattern));
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto subrng : new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view_single = vec | std::ranges::z::views::split(3);
        std::cout << "borrowed<new_view_single> = " << std::ranges::borrowed_range<decltype(new_view_single)> << "\n";
        std::cout << "sizeof(new_view_single.begin()) = "
                  << sizeof(new_view_single.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view_single)>);
        for (auto subrng : new_view_single) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = vec | std::ranges::z::views::split(pattern);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto subrng : fat_new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== join ===\n\n";

        using subrange_t = std::ranges::subrange<std::vector<int>::iterator>;
        std::vector<subrange_t> subranges = {
            {vec.begin(), vec.begin() + 2},
            {vec.begin() + 3, vec.begin() + 4},
            {vec.begin() + 4, vec.begin() + 6}};

        auto view = subranges | std::ranges::views::join;
        std::cout << "borrowed<view> = " << std::ranges::borrowed_range<decltype(view)> << "\n";
        std::cout << "sizeof(view.begin()) = " << sizeof(view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(view)>);
        for (auto x : view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    return 0;
}
