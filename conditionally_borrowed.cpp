// g++-13 -std=c++23 conditionally_borrowed.cpp -o borrowed

#include <array>
#include <iostream>
#include <ranges>
#include <vector>


namespace std::ranges::z {
  template<typename _F>
    constexpr bool __tidy_func =
      is_trivially_copyable_v<_F> && sizeof(_F) <= sizeof(void*);

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
    transform_view(_Range&&, _Fp) -> transform_view<views::all_t<_Range>, _Fp>;

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

    {
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
