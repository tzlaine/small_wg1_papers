// g++-13 -std=c++23 conditionally_borrowed.cpp -o borrowed

#include <array>
#include <iostream>
#include <ranges>
#include <vector>


namespace std::ranges::z {
  template<typename _F>
    constexpr bool __tidy_func =
      is_trivially_copyable_v<_F> && sizeof(_F) <= sizeof(void*);

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
    zip_view(_Rs&&...) -> zip_view<views::all_t<_Rs>...>;

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

    _F_access _M_f_access;

    __ziperator<_Const> _M_inner;

    const _Fp& __f() const
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

    _F_access _M_f_access;

    _InnerIter<_Const> _M_inner;

    const _Fp& __f() const
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

          _F_access _M_f_access;

          const _Fp& __f() const
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
}

template<typename... _Views>
constexpr bool
    std::ranges::enable_borrowed_range<std::ranges::z::zip_view<_Views...>> =
        (std::ranges::enable_borrowed_range<_Views> && ...);

template<typename _Fp, typename... _Views>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::zip_transform_view<_Fp, _Views...>> =
    (std::ranges::enable_borrowed_range<_Views> && ...) &&
    std::ranges::z::__tidy_func<_Fp>;

template<typename _Vp, size_t _Nm>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::adjacent_view<_Vp, _Nm>> =
    std::ranges::enable_borrowed_range<_Vp>;

template<typename _Vp, typename _Fp, size_t _Nm>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::adjacent_transform_view<_Vp, _Fp, _Nm>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Fp>;

template<typename _Vp, typename _Fp>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::transform_view<_Vp, _Fp>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Fp>;

template<typename _Vp, typename _Pred>
constexpr bool std::ranges::enable_borrowed_range<
    std::ranges::z::take_while_view<_Vp, _Pred>> =
    std::ranges::enable_borrowed_range<_Vp> && std::ranges::z::__tidy_func<_Pred>;


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


int main()
{
    std::vector<int> vec = {1, 2, 3, 4, 5, 6};
    std::vector<int> vec2 = {2, 3, 4, 5, 6, 1};

    {
        std::cout << "=== zip_transform ===\n\n";

        auto minus = [](auto x, auto y) { return x - y; };

        auto old_view = std::views::zip_transform(minus, vec, vec2);
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = std::ranges::z::views::zip_transform(minus, vec, vec2);
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = std::ranges::z::views::zip_transform(
            fat_callable(minus), vec, vec2);
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
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view =
            vec | std::ranges::z::views::adjacent_transform<2>(minus);
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = vec | std::ranges::z::views::adjacent_transform<2>(
                                      fat_callable(minus));
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
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::transform(negate);
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::transform(fat_callable(negate));
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== take_while ===\n\n";

        auto lt_four = [](auto x) { return x < 4; };

        auto old_view = vec | std::views::take_while(lt_four);
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::take_while(lt_four);
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::take_while(fat_callable(lt_four));
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    return 0;
}
