#include <algorithm>
#include <iterator>
#include <ranges>
#include <span>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#ifndef DO_TESTING
#define DO_TESTING 1
#endif

#ifndef USE_DONT_INVOKE
#define USE_DONT_INVOKE 0
#endif

#ifndef USE_AUTO_INVOKE
#define USE_AUTO_INVOKE 0
#endif

#if DO_TESTING
#include <gtest/gtest.h>

#if __has_include(<boost/mp11/list.hpp>)
#define HAVE_BOOST_MP11 1
#include <boost/mp11/list.hpp>
#else
#define HAVE_BOOST_MP11 0
#endif
#endif


namespace stdexec {
    namespace detail {
        template<typename T>
        constexpr bool has_type_params = false;
        template<template<class...> class Template, typename... Ts>
        constexpr bool has_type_params<Template<Ts...>> = true;

        template<auto X>
        struct constant_wrapper
        {
            static constexpr decltype(X) value = X;
        };
        template<auto X>
        constexpr auto cw = constant_wrapper<X>{};

        template<typename T>
        struct wrapper
        {
            using type = T;
        };
        template<typename T>
        consteval auto wrap()
        {
            return wrapper<T>{};
        }

        template<template<class...> class TypeList>
        struct temp_fold_base;
        template<
            template<class...>
            class TypeList,
            typename Prop,
            typename T,
            int I = 0,
            typename Tail = temp_fold_base<TypeList>>
        struct temp_fold;
        template<template<class...> class TypeList>
        struct temp_fold_base
        {
            consteval void prop_type() const {}
            constexpr void value() && {}
            template<typename T>
            static consteval bool contains(wrapper<T>)
            {
                return false;
            }

            template<typename Prop, typename T>
            constexpr auto operator()(Prop, T && x)
            {
                return temp_fold<TypeList, Prop, T>{
                    std::move(*this), std::move(x)};
            }
        };

        template<typename TempFolded, typename Prop>
        concept new_prop = (!TempFolded::contains(detail::wrap<Prop>()));

        template<
            template<class...>
            class TypeList,
            typename Prop,
            typename T,
            int I,
            typename Tail>
        struct temp_fold : Tail
        {
            temp_fold(Tail && tail, T && x) :
                Tail(std::move(tail)), x_(std::move(x))
            {}
            consteval wrapper<Prop> prop_type(constant_wrapper<I>) const
            {
                return {};
            }
            constexpr T && value(constant_wrapper<I>) &&
            {
                return std::move(x_);
            }
            static consteval bool contains(wrapper<Prop>) { return true; }

            template<typename Prop2, typename T2>
            constexpr auto operator()(Prop2, T2 && x)
            requires new_prop<temp_fold, Prop2>
            {
                return temp_fold<TypeList, Prop2, T2, I + 1, temp_fold>{
                    {std::move(*this)}, std::move(x)};
            }

            using Tail::prop_type;
            using Tail::value;
            using Tail::contains;

            T && x_;
        };

        struct type_fold_base;
        template<typename T, int I = 0, typename Tail = type_fold_base>
        struct type_fold;
        struct type_fold_base
        {
            consteval bool has_dupes() const { return false; }
            consteval void type() const {}
            template<typename T>
            consteval int index(wrapper<T>) const
            {
                return -1;
            }
            template<typename T>
            consteval bool contains(wrapper<T>) const
            {
                return false;
            }
            template<typename T>
            consteval auto operator+(wrapper<T>) const
            {
                return type_fold<T>{};
            }
        };
        template<typename T, int I, typename Tail>
        struct type_fold : Tail
        {
            consteval bool has_dupes() const { return sizeof(type_fold) != 1u; }
            consteval wrapper<T> type(constant_wrapper<I>) const { return {}; }
            consteval int index(wrapper<T>) const { return I; }
            consteval bool contains(wrapper<T> w) const { return true; }
            template<typename U>
            consteval auto operator+(wrapper<U>) const
            {
                return type_fold<U, I + 1, type_fold>{};
            }

            using Tail::has_dupes;
            using Tail::type;
            using Tail::index;
            using Tail::contains;

            [[no_unique_address]] wrapper<T> _;
        };

        template<template<typename...> typename TypeList>
        consteval auto to_type_fold(TypeList<>)
        {
            return type_fold_base{};
        }
        template<
            template<typename...>
            typename TypeList,
            typename T,
            typename... Ts>
        consteval auto to_type_fold(TypeList<T, Ts...>)
        {
            return (type_fold<T>{} + ... + detail::wrap<Ts>());
        }

        template<
            typename Prop,
            template<typename...>
            typename TypeList,
            typename T,
            typename... Ts>
        consteval int index_from_prop(TypeList<T, Ts...> tl)
        {
            return detail::to_type_fold(tl).index(detail::wrap<Prop>());
        }
    }

    template<typename T>
    concept type_list =
        std::is_empty_v<T> && std::semiregular<T> && detail::has_type_params<T>;

    namespace detail {
        template<typename T>
        concept non_void = !std::is_void_v<T>;
    }
    // clang-format off
    template<typename T, typename U>
    concept queryable_with = requires (T && env, U prop) {
        { ((T &&) env)[prop] } -> detail::non_void;
    };
    // clang-format on
    namespace detail {
        template<typename T, typename U>
        constexpr bool queryable_with_all = false;
        template<typename T, template<class...> class TypeList, typename... Ts>
        constexpr bool queryable_with_all<T, TypeList<Ts...>> =
            (queryable_with<T, Ts> && ...);
    }

    template<typename T>
    using properties_t = typename std::remove_cvref_t<T>::properties_type;

    // clang-format off
    template<typename T>
    concept environment = requires {
        typename properties_t<T>;
        requires type_list<properties_t<T>>;
    } && detail::queryable_with_all<T, properties_t<T>>;
    // clang-format on

#if USE_DONT_INVOKE
    template<typename T>
    struct dont_invoke
    {
        T value;
    };
#endif

    namespace detail{
        template<typename T>
        constexpr auto prop_to_value(T && x);
    }

    template<typename T>
    struct value_type_for
    {
        template<typename T_ = T>
        static T_ && operator()(T_ && x)
        {
            return (T_ &&) x;
        }
    };

    template<typename T>
    struct value_type_for<std::reference_wrapper<T>>
    {
        static auto operator()(std::reference_wrapper<T> ref)
        {
            return detail::prop_to_value(ref.get());
        }
    };

    template<typename CharT, typename Traits>
    struct value_type_for<std::basic_string_view<CharT, Traits>>
    {
        static auto operator()(std::basic_string_view<CharT, Traits> sv)
        {
            return std::basic_string<CharT, Traits>(sv);
        }
    };

    namespace detail {
        template<typename T>
        auto value_type_for_vec_transform(T const & from)
        {
            using value_type = decltype(detail::prop_to_value(*from.begin()));
            std::vector<value_type> retval;
            retval.reserve(from.size());
            std::ranges::transform(
                from, std::back_inserter(retval), [](auto & x) {
                    return detail::prop_to_value(x);
                });
            return retval;
        }
        template<size_t N, typename T>
        auto value_type_for_arr_transform(T const & from)
        {
            using value_type = decltype(detail::prop_to_value(*from.begin()));
            std::array<value_type, N> retval;
            std::ranges::transform(from, retval.begin(), [](auto & x) {
                return detail::prop_to_value(x);
            });
            return retval;
        }
    }

    template<typename T, size_t Extent>
    struct value_type_for<std::span<T, Extent>>
    {
        static auto operator()(std::span<T, Extent> span)
        {
            using value_type_before =
                std::ranges::range_value_t<std::vector<T>>;
            using value_type_after =
                std::remove_cvref_t<decltype(detail::prop_to_value(
                    *span.begin()))>;
            if constexpr (std::same_as<value_type_before, value_type_after>) {
                return span;
            } else {
                if constexpr (Extent == std::dynamic_extent)
                    return detail::value_type_for_vec_transform(span);
                else
                    return detail::value_type_for_arr_transform<Extent>(span);
            }
        }
    };

    template<typename T>
    struct value_type_for<std::vector<T>>
    {
        static auto operator()(std::vector<T> const & vec)
        {
            using value_type_before =
                std::ranges::range_value_t<std::vector<T>>;
            using value_type_after =
                std::remove_cvref_t<decltype(detail::prop_to_value(
                    *vec.begin()))>;
            if constexpr (std::same_as<value_type_before, value_type_after>) {
                return vec;
            } else {
                return detail::value_type_for_vec_transform(vec);
            }
        }
    };

    template<typename T, size_t N>
    struct value_type_for<std::array<T, N>>
    {
        static auto operator()(std::array<T, N> const & arr)
        {
            using value_type_before =
                std::ranges::range_value_t<std::array<T, N>>;
            using value_type_after =
                std::remove_cvref_t<decltype(detail::prop_to_value(
                    *arr.begin()))>;
            if constexpr (std::same_as<value_type_before, value_type_after>)
                return arr;
            else
                return detail::value_type_for_arr_transform<N>(arr);
        }
    };

    namespace detail {
#if USE_DONT_INVOKE
        template<typename T>
        constexpr bool is_dont_invoke = false;
        template<typename T>
        constexpr bool is_dont_invoke<dont_invoke<T>> = true;
#endif

        template<typename Prop, typename Props>
        constexpr bool has_type = 0 <= detail::index_from_prop<Prop>(Props{});

        template<typename T>
        constexpr int tl_size = -1;
        template<template<class...> class TypeList, typename... Ts>
        constexpr int tl_size<TypeList<Ts...>> = sizeof...(Ts);

        template<typename T, template<class...> class TypeList, typename... Ts>
        consteval auto tl_append(TypeList<Ts...>)
        {
            return TypeList<Ts..., T>{};
        }

        template<
            template<class...>
            class TypeList1,
            typename... Ts1,
            template<class...>
            class TypeList2,
            typename... Ts2>
        consteval auto tl_cat(TypeList1<Ts1...>, TypeList2<Ts2...>)
        {
            return TypeList1<Ts1..., Ts2...>{};
        }

        template<int JsOffset, int... Is, int... Js>
        consteval auto
        cat_indices(std::integer_sequence<int, Is...>, std::integer_sequence<int, Js...>)
        {
            return std::integer_sequence<int, Is..., Js + JsOffset...>{};
        }
        template<int I, int Size>
        consteval auto indices_without_i()
        {
            return detail::cat_indices<I + 1>(
                std::make_integer_sequence<int, I>{},
                std::make_integer_sequence<int, Size - (I + 1)>{});
        }

        template<template<class...> class TypeList, typename Folded, int... Is>
        consteval auto
        tl_without_i_impl(Folded f, std::integer_sequence<int, Is...>)
        {
            return TypeList<typename decltype(f.type(cw<Is>))::type...>{};
        }
        template<
            int I,
            typename Folded,
            template<class...>
            class TypeList,
            typename... Ts>
        consteval auto tl_without_i(Folded f, TypeList<Ts...> tl)
        {
            return detail::tl_without_i_impl<TypeList>(
                f, detail::indices_without_i<I, sizeof...(Ts)>());
        }

        template<
            template<typename...>
            typename TypeList,
            typename... Ts,
            typename... Us>
        consteval auto tl_like(TypeList<Ts...>, Us...)
        {
            return TypeList<Us...>{};
        }

        template<typename Tuple, int... Is>
        constexpr auto sub_tuple(Tuple && t, std::integer_sequence<int, Is...>)
        {
            return std::tuple(std::get<Is>((Tuple &&) t)...);
        }

        template<int I, typename Tuple>
        constexpr auto tuple_without_i(Tuple && t)
        {
            return detail::sub_tuple(
                (Tuple &&) t,
                detail::indices_without_i<
                    I,
                    std::tuple_size_v<std::remove_cvref_t<Tuple>>>());
        }

        template<
            template<class...>
            class TypeList,
            typename... Props2,
            typename... UniqueProps>
        consteval auto tl_set_diff_impl(
            TypeList<> tl1,
            TypeList<Props2...> tl2,
            TypeList<UniqueProps...> result)
        {
            return result;
        }
        template<
            template<class...>
            class TypeList,
            typename Prop,
            typename... Props1,
            typename... Props2,
            typename... UniqueProps>
        consteval auto tl_set_diff_impl(
            TypeList<Prop, Props1...> tl1,
            TypeList<Props2...> tl2,
            TypeList<UniqueProps...> result)
        {
            constexpr int i = index_from_prop<Prop>(tl2);
            if constexpr (0 <= i) {
                return detail::tl_set_diff_impl(
                    TypeList<Props1...>{}, tl2, TypeList<UniqueProps...>{});
            } else {
                return detail::tl_set_diff_impl(
                    TypeList<Props1...>{}, tl2, TypeList<UniqueProps..., Prop>{});
            }
        }
        template<
            template<class...>
            class TypeList1,
            typename... Props1,
            template<class...>
            class TypeList2,
            typename... Props2>
        consteval auto
        tl_set_diff(TypeList1<Props1...> tl1, TypeList2<Props2...> tl2)
        {
            return detail::tl_set_diff_impl(
                tl1, TypeList1<Props2...>{}, TypeList1<>{});
        }

        template<
            typename... Ts,
            template<class...>
            class TypeList,
            typename... Props>
        constexpr bool
        same_arity_impl(TypeList<Props...> tl, wrapper<std::tuple<Ts...>> w)
        {
            return sizeof...(Ts) == sizeof...(Props);
        }
        template<typename Props, typename Tuple>
        constexpr bool
            same_arity = detail::same_arity_impl(Props{}, detail::wrap<Tuple>());

        template<typename T>
        constexpr bool is_tuple_v = false;
        template<typename... Ts>
        constexpr bool is_tuple_v<std::tuple<Ts...>> = true;
        template<typename T>
        concept is_tuple = is_tuple_v<T>;

        template<typename T>
        constexpr auto prop_to_value(T && x)
        {
            using just_t = std::remove_cvref_t<T>;
#if USE_DONT_INVOKE
            if constexpr (is_dont_invoke<just_t>) {
                return (T &&) x;
            } else {
                return value_type_for<just_t>{}((T &&) x);
            }
#else
            return value_type_for<just_t>{}((T &&) x);
#endif
        }
    }

    template<typename T, typename Types>
    concept in_type_list = detail::has_type<T, Types>;

    template<typename... Ts>
    struct types
    {};

    template<type_list Properties, typename Tuple>
    requires detail::same_arity<Properties, Tuple> && detail::is_tuple<Tuple>
    struct env;

    namespace detail {
        template<
            template<class...>
            class TypeList,
            typename Prop,
            typename T,
            int I,
            typename Tail,
            int... Is>
        auto temp_fold_props(
            temp_fold<TypeList, Prop, T, I, Tail> const & folded,
            std::integer_sequence<int, Is...>)
        {
            return TypeList<typename decltype(folded.prop_type(
                cw<Is>))::type...>{};
        }

        template<
            template<class...>
            class TypeList,
            typename Prop,
            typename T,
            int I,
            typename Tail,
            int... Is>
        auto temp_fold_values(
            temp_fold<TypeList, Prop, T, I, Tail> && folded,
            std::integer_sequence<int, Is...>)
        {
            return std::tuple(std::move(folded).value(cw<Is>)...);
        }

#if USE_AUTO_INVOKE
        template<typename T>
        concept nonvoid_nullary_invocable = std::invocable<T> &&
            (!std::is_void_v<std::invoke_result_t<T>>);
        template<typename T, typename Prop>
        concept nonvoid_prop_invocable = std::invocable<T, Prop> &&
            (!std::is_void_v<std::invoke_result_t<T, Prop>>);

        template<typename Properties, typename Tuple, typename Prop>
        decltype(auto) get_no_invoke(Tuple && tuple)
        {
            constexpr size_t i = detail::index_from_prop<Prop>(Properties{});
            return std::get<i>((Tuple &&) tuple);
        }

        template<int... Is, typename Tuple>
        auto tuple_deep_copy(std::integer_sequence<int, Is...>, Tuple && tuple)
        {
            return std::tuple(
                detail::prop_to_value(std::get<Is>((Tuple &&) tuple))...);
        }

        template<typename Properties, typename Tuple>
        auto make_env(Properties props, Tuple && values)
        {
            return env(props, std::move(values));
        }
#endif
    }

    template<type_list Properties, typename Tuple>
    requires detail::same_arity<Properties, Tuple> && detail::is_tuple<Tuple>
    struct env
    {
        using properties_type = Properties;
        using tuple_type = Tuple;

        // clang-format off
        constexpr env()
            requires std::default_initializable<Tuple> = default;
        constexpr env(env const & other)
            requires std::copy_constructible<Tuple> = default;
        constexpr env(env && other)
            requires std::move_constructible<Tuple> = default;
        constexpr env & operator=(env const & other)
            requires std::assignable_from<Tuple &, Tuple const &> = default;
        constexpr env & operator=(env && other)
            requires std::assignable_from<Tuple &, Tuple &&> = default;
        // clang-format on

        constexpr env(Properties props, Tuple const & tup) :
            properties(props), values(tup)
        {}
        constexpr env(Properties props, Tuple && tup) :
            properties(props), values(std::move(tup))
        {}

        template<
            template<class...>
            class TypeList,
            typename Prop,
            typename T,
            int I,
            typename Tail>
        constexpr env(detail::temp_fold<TypeList, Prop, T, I, Tail> && folded) :
            properties(detail::temp_fold_props(
                folded, std::make_integer_sequence<int, I + 1>{})),
            values(detail::temp_fold_values(
                std::move(folded), std::make_integer_sequence<int, I + 1>{}))
        {}

        // clang-format off
        template<typename Properties2, typename Tuple2>
        constexpr bool operator==(env<Properties2, Tuple2> const & other) const
        requires std::same_as<Properties, Properties2> &&
                 std::equality_comparable_with<Tuple, Tuple2>
        // clang-format on
        {
            return values == other.values;
        }

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<Properties> Prop>
        constexpr decltype(auto) operator[](this Self && self, Prop p)
        {
            constexpr size_t i = detail::index_from_prop<Prop>(Properties{});
#if USE_AUTO_INVOKE
            using result_type = std::remove_cvref_t<decltype(std::get<i>(
                ((Self &&) self).values))>;
#if USE_DONT_INVOKE
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(((Self &&) self).values).value;
            } else
#endif
                if constexpr (detail::nonvoid_nullary_invocable<result_type>) {
                return std::get<i>(((Self &&) self).values)();
            } else if constexpr (detail::nonvoid_prop_invocable<
                                     result_type,
                                     Prop>) {
                return std::get<i>(((Self &&) self).values)(t);
            } else {
                return std::get<i>(((Self &&) self).values);
            }
#else
            return std::get<i>(((Self &&) self).values);
#endif
        }
#else
        template<in_type_list<Properties> Prop>
        constexpr decltype(auto) operator[](Prop p) &
        {
            constexpr size_t i = detail::index_from_prop<Prop>(Properties{});
#if USE_AUTO_INVOKE
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(values))>;
#if USE_DONT_INVOKE
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(values).value;
            } else
#endif
                if constexpr (detail::nonvoid_nullary_invocable<result_type>) {
                return std::get<i>(values)();
            } else if constexpr (detail::nonvoid_prop_invocable<
                                     result_type,
                                     Prop>) {
                return std::get<i>(values)(p);
            } else {
                return std::get<i>(values);
            }
#else
            return std::get<i>(values);
#endif
        }
        template<in_type_list<Properties> Prop>
        constexpr decltype(auto) operator[](Prop p) const &
        {
            constexpr size_t i = detail::index_from_prop<Prop>(Properties{});
#if USE_AUTO_INVOKE
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(values))>;
#if USE_DONT_INVOKE
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(values).value;
            } else
#endif
                if constexpr (detail::nonvoid_nullary_invocable<result_type>) {
                return std::get<i>(values)();
            } else if constexpr (detail::nonvoid_prop_invocable<
                                     result_type,
                                     Prop>) {
                return std::get<i>(values)(p);
            } else {
                return std::get<i>(values);
            }
#else
            return std::get<i>(values);
#endif
       }
        template<in_type_list<Properties> Prop>
        constexpr decltype(auto) operator[](Prop p) &&
        {
            constexpr size_t i = detail::index_from_prop<Prop>(Properties{});
#if USE_AUTO_INVOKE
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(std::move(values)))>;
#if USE_DONT_INVOKE
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(std::move(values)).value;
            } else
#endif
                if constexpr (detail::nonvoid_nullary_invocable<result_type>) {
                return std::get<i>(std::move(values))();
            } else if constexpr (detail::nonvoid_prop_invocable<
                                     result_type,
                                     Prop>) {
                return std::get<i>(std::move(values))(p);
            } else {
                return std::get<i>(std::move(values));
            }
#else
            return std::get<i>(std::move(values));
#endif
        }
        template<in_type_list<Properties> Prop>
        constexpr decltype(auto) operator[](Prop p) const &&
        {
            constexpr size_t i = detail::index_from_prop<Prop>(Properties{});
#if USE_AUTO_INVOKE
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(std::move(values)))>;
#if USE_DONT_INVOKE
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(std::move(values)).value;
            } else
#endif
                if constexpr (detail::nonvoid_nullary_invocable<result_type>) {
                return std::get<i>(std::move(values))();
            } else if constexpr (detail::nonvoid_prop_invocable<
                                     result_type,
                                     Prop>) {
                return std::get<i>(std::move(values))(p);
            } else {
                return std::get<i>(std::move(values));
            }
#else
            return std::get<i>(std::move(values));
#endif
        }
#endif

#if USE_AUTO_INVOKE
#if defined(__cpp_explicit_this_parameter)
        template<typename Self>
        constexpr auto deep_copy(this Self && self)
        {
            return detail::make_env(
                properties,
                detail::tuple_deep_copy(
                    std::make_integer_sequence<int, std::tuple_size_v<Tuple>>{},
                    ((Self &&) self)).values));
        }
#else
        constexpr auto deep_copy() const &
        {
            return detail::make_env(
                properties,
                detail::tuple_deep_copy(
                    std::make_integer_sequence<int, std::tuple_size_v<Tuple>>{},
                    values));
        }
        auto deep_copy() &&
        {
            return detail::make_env(
                properties,
                detail::tuple_deep_copy(
                    std::make_integer_sequence<int, std::tuple_size_v<Tuple>>{},
                    std::move(values)));
        }
        auto deep_copy() const && = delete;
#endif
#endif

        [[no_unique_address]] Properties properties;
        Tuple values;
    };

    template<
        template<class...>
        class TypeList,
        typename Prop,
        typename T,
        int I,
        typename Tail>
    env(detail::temp_fold<TypeList, Prop, T, I, Tail> && folded) -> env<
        decltype(detail::temp_fold_props(
            std::declval<detail::temp_fold<TypeList, Prop, T, I, Tail>>(),
            std::make_integer_sequence<int, I + 1>{})),
        decltype(detail::temp_fold_values(
            std::declval<detail::temp_fold<TypeList, Prop, T, I, Tail>>(),
            std::make_integer_sequence<int, I + 1>{}))>;

    inline constexpr env<types<>, std::tuple<>> empty_env;

    inline detail::temp_fold_base<types> make_env;

    template<template<class...> class TypeList>
    detail::temp_fold_base<TypeList> make_env_with;

    template<size_t I, typename Properties, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Properties, Tuple> & env)
    {
        return std::get<I>(env.values);
    }
    template<size_t I, typename Properties, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Properties, Tuple> const & env)
    {
        return std::get<I>(env.values);
    }
    template<size_t I, typename Properties, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Properties, Tuple> && env)
    {
        return std::get<I>(std::move(env.values));
    }
    template<size_t I, typename Properties, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Properties, Tuple> const && env)
    {
        return std::get<I>(std::move(env.values));
    }

    template<environment Env, typename Prop>
    constexpr bool contains(Env const &, Prop)
    {
        return in_type_list<Prop, properties_t<Env>>;
    }

    template<environment Env, typename Prop>
    constexpr bool contains(Prop)
    {
        return in_type_list<Prop, properties_t<Env>>;
    }

    template<
        environment Env,
        template<class...>
        class TypeList,
        typename... Props2>
    requires type_list<TypeList<Props2...>>
    constexpr bool contains_all_of(Env const &, TypeList<Props2...>)
    {
        return (in_type_list<Props2, properties_t<Env>> && ...);
    }

    template<
        environment Env,
        template<class...>
        class TypeList,
        typename... Props2>
    constexpr bool contains_all_of(TypeList<Props2...>)
    {
        return (in_type_list<Props2, properties_t<Env>> && ...);
    }

    template<typename T, typename Env>
    concept property_of = environment<Env> &&
        in_type_list<T, typename Env::properties_type>; // TODO properties_t<Env>>;

    // clang-format off
    template<typename Prop, environment Env>
    requires property_of<Prop, Env>
    constexpr decltype(auto) get(Env & e)
    {
        return e[Prop{}];
    }
    template<typename Prop, environment Env>
    requires property_of<Prop, Env>
    constexpr decltype(auto) get(Env const & e)
    {
        return e[Prop{}];
    }
    template<typename Prop, environment Env>
    requires property_of<Prop, Env>
    constexpr decltype(auto) get(Env && e)
    {
        return std::move(e)[Prop{}];
    }
    template<typename Prop, environment Env>
    requires property_of<Prop, Env>
    constexpr decltype(auto) get(Env const && e)
    {
        return std::move(e)[Prop{}];
    }
    // clang-format on

    // clang-format off
    template<typename Prop, environment Env, typename T>
    requires (!property_of<Prop, Env>)
    constexpr auto insert(Env const & e, Prop, T && x)
    {
        using props_t = properties_t<Env>;
        return env(
            detail::tl_append<std::remove_cvref_t<Prop>>(props_t{}),
            tuple_cat(e.values, std::tuple((T &&) x)));
    }
    template<typename Prop, environment Env, typename T>
    requires (!property_of<Prop, Env>)
    constexpr auto insert(Env && e, Prop, T && x)
    {
        using props_t = properties_t<Env>;
        return env(
            detail::tl_append<Prop>(props_t{}),
            tuple_cat(std::move(e.values), std::tuple((T &&) x)));
    }
    // clang-format on

    // clang-format off
    template<
        environment Env,
        template<typename...>
        typename TypeList,
        property_of<Env>... Props>
    requires type_list<TypeList<Props...>>
    constexpr decltype(auto) slice(Env const & e, TypeList<Props...> props)
    // clang-format on
    {
        return env(props, std::tuple(e[Props{}]...));
    }

    // clang-format off
    template<
        environment Env,
        template<typename...>
        typename TypeList,
        property_of<Env>... Props>
    requires type_list<TypeList<Props...>>
    constexpr decltype(auto) slice(Env && e, TypeList<Props...> props)
    // clang-format on
    {
        return env(props, std::tuple(std::move(e)[Props{}]...));
    }

    template<environment Env, property_of<Env>... Props>
    constexpr decltype(auto) slice(Env const & e, Props...)
    {
        using props_t = properties_t<Env>;
        return stdexec::slice(e, detail::tl_like(props_t{}, Props{}...));
    }

    template<environment Env, property_of<Env>... Props>
    constexpr decltype(auto) slice(Env && e, Props...)
    {
        using props_t = properties_t<Env>;
        return stdexec::slice(
            std::move(e), detail::tl_like(props_t{}, Props{}...));
    }

    namespace detail {
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Props1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Props2>
        constexpr auto make_env_tuple(
            Env1 const & env1,
            TypeList1<Props1...> old_props,
            Env2 const & env2,
            TypeList2<Props2...>)
        {
            return std::tuple(
                stdexec::get<Props1>(env1)..., stdexec::get<Props2>(env2)...);
        }
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Props1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Props2>
        constexpr auto make_env_tuple(
            Env1 && env1,
            TypeList1<Props1...> old_props,
            Env2 const & env2,
            TypeList2<Props2...>)
        {
            return std::tuple(
                stdexec::get<Props1>(std::move(env1))...,
                stdexec::get<Props2>(env2)...);
        }
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Props1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Props2>
        constexpr auto make_env_tuple(
            Env1 const & env1,
            TypeList1<Props1...> old_props,
            Env2 && env2,
            TypeList2<Props2...>)
        {
            return std::tuple(
                stdexec::get<Props1>(env1)...,
                stdexec::get<Props2>(std::move(env2))...);
        }
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Props1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Props2>
        constexpr auto make_env_tuple(
            Env1 && env1,
            TypeList1<Props1...> old_props,
            Env2 && env2,
            TypeList2<Props2...>)
        {
            return std::tuple(
                stdexec::get<Props1>(std::move(env1))...,
                stdexec::get<Props2>(std::move(env2))...);
        }
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 const & e1, Env2 const & e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto old_props = detail::tl_set_diff(props1{}, props2{});
        return env(
            detail::tl_cat(old_props, props2{}),
            detail::make_env_tuple(e1, old_props, e2, props2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 && e1, Env2 const & e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto old_props = detail::tl_set_diff(props1{}, props2{});
        return env(
            detail::tl_cat(old_props, props2{}),
            detail::make_env_tuple(std::move(e1), old_props, e2, props2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 const & e1, Env2 && e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto old_props = detail::tl_set_diff(props1{}, props2{});
        return env(
            detail::tl_cat(old_props, props2{}),
            detail::make_env_tuple(e1, old_props, std::move(e2), props2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 && e1, Env2 && e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto old_props = detail::tl_set_diff(props1{}, props2{});
        return env(
            detail::tl_cat(old_props, props2{}),
            detail::make_env_tuple(
                std::move(e1), old_props, std::move(e2), props2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 const & e1, Env2 const & e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto new_props = detail::tl_set_diff(props2{}, props1{});
        return env(
            detail::tl_cat(props1{}, new_props),
            detail::make_env_tuple(e1, props1{}, e2, new_props));
    }
    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 const & e1, Env2 && e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto new_props = detail::tl_set_diff(props2{}, props1{});
        return env(
            detail::tl_cat(props1{}, new_props),
            detail::make_env_tuple(e1, props1{}, std::move(e2), new_props));
    }
    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 && e1, Env2 const & e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto new_props = detail::tl_set_diff(props2{}, props1{});
        return env(
            detail::tl_cat(props1{}, new_props),
            detail::make_env_tuple(std::move(e1), props1{}, e2, new_props));
    }
    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 && e1, Env2 && e2)
    {
        using props1 = properties_t<Env1>;
        using props2 = properties_t<Env2>;
        constexpr auto new_props = detail::tl_set_diff(props2{}, props1{});
        return env(
            detail::tl_cat(props1{}, new_props),
            detail::make_env_tuple(
                std::move(e1), props1{}, std::move(e2), new_props));
    }

    template<typename Prop, environment Env>
    requires property_of<Prop, Env>
    constexpr auto erase(Env const & e, Prop)
    {
        using props_t = properties_t<Env>;
        constexpr auto folded = detail::to_type_fold(props_t{});
        constexpr int i = folded.index(detail::wrap<Prop>());
        return env(
            detail::tl_without_i<i>(folded, props_t{}),
            detail::tuple_without_i<i>(e.values));
    }
    template<typename Prop, environment Env>
    requires property_of<Prop, Env>
    constexpr auto erase(Env && e, Prop)
    {
        using props_t = properties_t<Env>;
        constexpr auto folded = detail::to_type_fold(props_t{});
        constexpr int i = folded.index(detail::wrap<Prop>());
        return env(
            detail::tl_without_i<i>(folded, props_t{}),
            detail::tuple_without_i<i>(std::move(e.values)));
    }

    template<
        environment Env,
        template<class...>
        class TypeList,
        property_of<Env>... Props>
    constexpr auto erase(Env const & e, TypeList<Props...>)
    {
        using props_t = properties_t<Env>;
        constexpr auto remaining_props =
            detail::tl_set_diff(props_t{}, types<Props...>{});
        return stdexec::slice(e, remaining_props);
    }

    template<
        environment Env,
        template<class...>
        class TypeList,
        property_of<Env>... Props>
    constexpr auto erase(Env && e, TypeList<Props...>)
    {
        using props_t = properties_t<Env>;
        constexpr auto remaining_props =
            detail::tl_set_diff(props_t{}, types<Props...>{});
        return stdexec::slice(std::move(e), remaining_props);
    }

    template<environment Env, property_of<Env>... Props>
    constexpr auto erase(Env const & e, Props...)
    {
        return stdexec::erase(e, types<Props...>{});
    }

    template<environment Env, property_of<Env>... Props>
    constexpr auto erase(Env && e, Props...)
    {
        return stdexec::erase(std::move(e), types<Props...>{});
    }

#if USE_AUTO_INVOKE
    template<typename Properties, typename Tuple>
    constexpr auto deep_copy(env<Properties, Tuple> const & e)
    {
        return e.deep_copy();
    }

    template<typename Properties, typename Tuple>
    constexpr auto deep_copy(env<Properties, Tuple> && e)
    {
        return std::move(e).deep_copy();
    }
#endif

    namespace detail {
        template<
            typename Env,
            template<typename...>
            typename TypeList,
            typename... Ts>
        constexpr auto deep_copy_impl(Env && e, TypeList<Ts...> tl)
        {
            return env(
                tl, std::tuple(detail::prop_to_value(((Env &&) e)[Ts{}])...));
        }
    }

    template<environment Env>
    constexpr auto deep_copy(Env && e)
    {
        using props_t = properties_t<Env>;
        return detail::deep_copy_impl((Env &&) e, props_t{});
    }



    // second-order envs

    namespace detail {
        // clang-format off
        template<typename Lhs, typename Rhs, typename Env>
        concept pipe_invocable = requires {
            std::declval<Rhs>()(std::declval<Lhs>()(std::declval<Env>()));
        };
        // clang-format on
        template<typename Lhs, typename Rhs>
        struct pipe
        {
            constexpr pipe(Lhs lhs, Rhs rhs) :
                lhs_(std::move(lhs)), rhs_(std::move(rhs))
            {}

            template<typename Env>
            requires pipe_invocable<const Lhs &, const Rhs &, Env>
            constexpr auto operator()(Env && env) const &
            {
                return rhs_(lhs_((Env &&) env));
            }

            template<typename Env>
            requires pipe_invocable<Lhs, Rhs, Env>
            constexpr auto operator()(Env && env) &&
            {
                return std::move(rhs_)(std::move(lhs_)((Env &&) env));
            }

            template<typename Env>
            constexpr auto operator()(Env && env) const && = delete;

            [[no_unique_address]] Lhs lhs_;
            [[no_unique_address]] Rhs rhs_;
        };

        struct env_adaptor_closure_impl
        {
            // clang-format off
            template<typename Env, typename Self>
            requires
                std::derived_from<std::remove_cvref_t<Self>,
                                  env_adaptor_closure_impl> &&
                std::invocable<Self, Env>
            friend constexpr auto operator|(Env && env, Self && self)
            // clang-format on
            {
                return ((Self &&) self)((Env &&) env);
            }

            template<typename Lhs, typename Rhs>
            requires std::derived_from<Lhs, env_adaptor_closure_impl> &&
                std::derived_from<Rhs, env_adaptor_closure_impl>
            friend constexpr auto operator|(Lhs lhs, Rhs rhs)
            {
                return detail::pipe<Lhs, Rhs>{std::move(lhs), std::move(rhs)};
            }
        };

        template<typename Func, typename... CapturedArgs>
        struct bind_back_t
        {
            static_assert(std::is_move_constructible<Func>::value, "");
            static_assert(
                (std::is_move_constructible<CapturedArgs>::value && ...), "");

            template<typename F, typename... Args>
            explicit constexpr bind_back_t(int, F && f, Args &&... args) :
                f_((F &&) f), bound_args_((Args &&) args...)
            {
                static_assert(sizeof...(Args) == sizeof...(CapturedArgs), "");
            }

            template<typename... Args>
            constexpr decltype(auto) operator()(Args &&... args) &
            {
                return call_impl(*this, indices(), (Args &&) args...);
            }

            template<typename... Args>
            constexpr decltype(auto) operator()(Args &&... args) const &
            {
                return call_impl(*this, indices(), (Args &&) args...);
            }

            template<typename... Args>
            constexpr decltype(auto) operator()(Args &&... args) &&
            {
                return call_impl(
                    std::move(*this), indices(), (Args &&) args...);
            }

            template<typename... Args>
            constexpr decltype(auto) operator()(Args &&... args) const &&
            {
                return call_impl(
                    std::move(*this), indices(), (Args &&) args...);
            }

        private:
            using indices = std::index_sequence_for<CapturedArgs...>;

            template<typename T, size_t... I, typename... Args>
            static constexpr decltype(auto)
            call_impl(T && this_, std::index_sequence<I...>, Args &&... args)
            {
                return ((T &&) this_)
                    .f_((Args &&) args...,
                        std::get<I>(((T &&) this_).bound_args_)...);
            }

            Func f_;
            std::tuple<CapturedArgs...> bound_args_;
        };

        template<typename Func, typename... Args>
        using bind_back_result =
            bind_back_t<std::decay_t<Func>, std::decay_t<Args>...>;

        template<typename Func, typename... Args>
        constexpr auto bind_back(Func && f, Args &&... args)
        {
            return bind_back_result<Func, Args...>(
                0, (Func &&) f, (Args &&) args...);
        }

        template<typename F>
        struct closure : env_adaptor_closure_impl
        {
            constexpr closure(F f) : f_(f) {}

            template<typename T>
            requires std::invocable<F const &, T>
            constexpr decltype(auto) operator()(T && t) const &
            {
                return f_((T &&) t);
            }

            template<typename T>
            requires std::invocable<F &&, T>
            constexpr decltype(auto) operator()(T && t) &&
            {
                return std::move(f_)((T &&) t);
            }

        private:
            F f_;
        };

        template<typename F>
        struct adaptor
        {
            constexpr adaptor(F f) : f_(f) {}

            template<typename... Args>
            constexpr auto operator()(Args &&... args) const
            {
                if constexpr (std::is_invocable_v<F const &, Args...>) {
                    return f_((Args &&) args...);
                } else {
                    return closure(detail::bind_back(f_, (Args &&) args...));
                }
            }

        private:
            F f_;
        };
    }

    template<typename D>
    requires std::is_class_v<D> && std::same_as<D, std::remove_cv_t<D>>
    struct env_adaptor_closure : detail::env_adaptor_closure_impl
    {};

    template<environment E>
    struct ref_env
    {
        using properties_type = properties_t<E>;

        ref_env() = default;
        ref_env(E & base) : base_(std::addressof(base)) {}
        ref_env(std::reference_wrapper<E> ref) : ref_env(ref.get()) {}

        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p)
        {
            return (*base_)[p];
        }

        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const
        {
            return std::as_const(*base_)[p];
        }

    private:
        E * base_ = nullptr;
    };

    template<environment E>
    ref_env(E &) -> ref_env<E>;
    template<environment E>
    ref_env(E const &) -> ref_env<E const>;
    template<environment E>
    ref_env(std::reference_wrapper<E>) -> ref_env<E>;

    namespace detail {
        template<typename F, typename Props>
        constexpr bool invocable_with_all = false;
        template<
            typename F,
            template<typename...>
            typename TypeList,
            typename... Ts>
        constexpr bool invocable_with_all<F, TypeList<Ts...>> =
            (std::invocable<F, Ts> && ...);
    }

    template<typename F, type_list Props>
    requires detail::invocable_with_all<F, Props>
    struct computed_env
    {
        using properties_type = Props;

        // clang-format off
        computed_env() requires std::default_initializable<F> = default;
        // clang-format on
        explicit computed_env(F f, Props props) : f_(f) {}

        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p)
        {
            return f_(p);
        }

        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const
        {
            return f_(p);
        }

    private:
        F f_ = F();
    };

    template<environment E1, environment E2>
    struct layer_env
    {
        using properties_type = decltype(detail::tl_cat(
            properties_t<E1>{},
            detail::tl_set_diff(properties_t<E2>{}, properties_t<E1>{})));

        // clang-format off
        layer_env()
        requires std::default_initializable<E1> &&
                 std::default_initializable<E2>
        = default;
        // clang-format on
        layer_env(E1 base_1, E2 base_2) :
            base_1_(std::move(base_1)), base_2_(std::move(base_2))
        {}

        E1 const & base_1() const & requires std::copy_constructible<E1>
        {
            return base_1_;
        }
        E1 base_1() && { return std::move(base_1_); }

        E2 const & base_2() const & requires std::copy_constructible<E2>
        {
            return base_2_;
        }
        E2 base_2() && { return std::move(base_2_); }

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](this Self && self, Prop p)
        {
            if constexpr (in_type_list<Prop, properties_t<E1>>)
                return ((Self &&) self).base_1_[p];
            else
                return ((Self &&) self).base_2_[p];
        }
#else
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) &
        {
            if constexpr (in_type_list<Prop, properties_t<E1>>)
                return base_1_[p];
            else
                return base_2_[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const &
        {
            if constexpr (in_type_list<Prop, properties_t<E1>>)
                return base_1_[p];
            else
                return base_2_[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) &&
        {
            if constexpr (in_type_list<Prop, properties_t<E1>>)
                return std::move(base_1_)[p];
            else
                return std::move(base_2_)[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const &&
        {
            if constexpr (in_type_list<Prop, properties_t<E1>>)
                return std::move(base_1_)[p];
            else
                return std::move(base_2_)[p];
        }
#endif

    private:
        E1 base_1_ = E1();
        E2 base_2_ = E2();
    };

    template<typename E1, typename E2>
    layer_env(E1, E2) -> layer_env<E1, E2>;
    template<typename E1, typename E2>
    layer_env(std::reference_wrapper<E1>, E2) -> layer_env<ref_env<E1>, E2>;
    template<typename E1, typename E2>
    layer_env(E1, std::reference_wrapper<E2>) -> layer_env<E1, ref_env<E2>>;
    template<typename E1, typename E2>
    layer_env(std::reference_wrapper<E1>, std::reference_wrapper<E2>)
        -> layer_env<ref_env<E1>, ref_env<E2>>;

    namespace detail {
        // clang-format off
        template<typename T>
        concept can_ref_env = requires { ref_env(std::declval<T>()); };

        template<typename T>
        concept env_or_ref = environment<T> || can_ref_env<T>;

        template<typename T, typename U>
        concept can_layer_env = requires {
            layer_env(std::declval<T>(), std::declval<U>());
        };
        // clang-format on

        template<typename T>
        constexpr bool is_ref_wrapper = false;
        template<typename T>
        constexpr bool is_ref_wrapper<std::reference_wrapper<T>> = true;

        struct layer_impl
        {
            template<env_or_ref E1, env_or_ref E2>
            requires can_layer_env<E1, E2>
            [[nodiscard]] constexpr auto operator()(E1 && e1, E2 && e2) const
            {
                return layer_env((E1 &&) e1, (E2 &&) e2);
            }
        };
    }

    inline constexpr detail::adaptor<detail::layer_impl>
        layer(detail::layer_impl{});

    // clang-format off
    template<environment E, type_list Props>
    requires (contains_all_of<E>(Props{}))
    struct with_only_env
    // clang-format on
    {
        using properties_type = Props;

        // clang-format off
        with_only_env() requires std::default_initializable<E> = default;
        // clang-format on
        with_only_env(E base, Props props) : base_(std::move(base)) {}

        E const & base() const & requires std::copy_constructible<E>
        {
            return base_;
        }
        E base() && { return std::move(base_); }

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](this Self && self, Prop p)
        {
            return ((Self &&) self).base_[p];
        }
#else
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) &
        {
            return base_[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const &
        {
            return base_[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) &&
        {
            return std::move(base_)[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const &&
        {
            return std::move(base_)[p];
        }
#endif

    private:
        E base_ = E();
    };

    template<typename E, typename Props>
    with_only_env(E, Props) -> with_only_env<E, Props>;
    template<typename E, typename Props>
    with_only_env(std::reference_wrapper<E>, Props)
        -> with_only_env<ref_env<E>, Props>;

    namespace detail {
        // clang-format off
        template<typename T, typename Props>
        concept can_with_only_env = requires {
            with_only_env(std::declval<T>(), Props{});
        };
        // clang-format on

        struct with_only_impl : env_adaptor_closure<with_only_impl>
        {
            template<env_or_ref E, type_list Props>
            requires can_with_only_env<E, Props>
            constexpr auto operator()(E && e, Props props) const
            {
                return with_only_env((E &&) e, props);
            }
        };
    }

    inline constexpr detail::adaptor<detail::with_only_impl>
        with_only(detail::with_only_impl{});

    template<environment E, type_list Props>
    struct without_env
    {
        using properties_type =
            decltype(detail::tl_set_diff(properties_t<E>{}, Props{}));

        // clang-format off
        without_env() requires std::default_initializable<E> = default;
        // clang-format on
        without_env(E base, Props props) : base_(std::move(base)) {}

        E const & base() const & requires std::copy_constructible<E>
        {
            return base_;
        }
        E base() && { return std::move(base_); }

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](this Self && self, Prop p)
        {
            return ((Self &&) self).base_[p];
        }
#else
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) &
        {
            return base_[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const &
        {
            return base_[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) &&
        {
            return std::move(base_)[p];
        }
        template<in_type_list<properties_type> Prop>
        constexpr decltype(auto) operator[](Prop p) const &&
        {
            return std::move(base_)[p];
        }
#endif

    private:
        E base_ = E();
    };

    template<typename E, typename Props>
    without_env(E, Props) -> without_env<E, Props>;
    template<typename E, typename Props>
    without_env(std::reference_wrapper<E>, Props)
        -> without_env<ref_env<E>, Props>;

    namespace detail {
        // clang-format off
        template<typename T, typename Props>
        concept can_without_env = requires {
            without_env(std::declval<T>(), Props{});
        };
        // clang-format on

        struct without_impl : env_adaptor_closure<without_impl>
        {
            template<env_or_ref E, type_list Props>
            requires can_without_env<E, Props>
            constexpr auto operator()(E && e, Props props) const
            {
                return without_env((E &&) e, props);
            }
        };
    }

    inline constexpr detail::adaptor<detail::without_impl>
        without(detail::without_impl{});
}

#if DO_TESTING

struct int_prop
{};
struct double_prop
{};
struct string_prop
{};

struct int_2_prop
{};
struct double_2_prop
{};
struct string_2_prop
{};

struct nullary_func_prop
{};
struct unary_func_prop
{};
struct binary_func_prop
{};

static_assert(
    !stdexec::detail::to_type_fold(stdexec::types<int_prop, double_prop>{})
         .has_dupes());
static_assert(stdexec::detail::to_type_fold(
                  stdexec::types<int_prop, string_prop, int_prop>{})
                  .has_dupes());
static_assert(!stdexec::detail::to_type_fold(
                   stdexec::types<int_prop, string_prop, double_prop>{})
                   .has_dupes());

static_assert(
    stdexec::detail::to_type_fold(
        stdexec::types<int_prop, string_prop, double_prop>{})
        .index(stdexec::detail::wrap<int_prop>()) == 0);
static_assert(
    stdexec::detail::to_type_fold(
        stdexec::types<int_prop, string_prop, double_prop>{})
        .index(stdexec::detail::wrap<string_prop>()) == 1);
static_assert(
    stdexec::detail::to_type_fold(
        stdexec::types<int_prop, string_prop, double_prop>{})
        .index(stdexec::detail::wrap<double_prop>()) == 2);
static_assert(
    stdexec::detail::to_type_fold(
        stdexec::types<int_prop, string_prop, double_prop>{})
        .index(stdexec::detail::wrap<int>()) == -1);

static_assert(stdexec::detail::to_type_fold(
                  stdexec::types<int_prop, string_prop, double_prop>{})
                  .contains(stdexec::detail::wrap<int_prop>()));
static_assert(stdexec::detail::to_type_fold(
                  stdexec::types<int_prop, string_prop, double_prop>{})
                  .contains(stdexec::detail::wrap<string_prop>()));
static_assert(stdexec::detail::to_type_fold(
                  stdexec::types<int_prop, string_prop, double_prop>{})
                  .contains(stdexec::detail::wrap<double_prop>()));
static_assert(!stdexec::detail::to_type_fold(
                   stdexec::types<int_prop, string_prop, double_prop>{})
                   .contains(stdexec::detail::wrap<int>()));

static_assert(std::same_as<
              decltype(stdexec::detail::to_type_fold(
                           stdexec::types<int_prop, string_prop, double_prop>{})
                           .type(stdexec::detail::cw<0>))::type,
              int_prop>);
static_assert(std::same_as<
              decltype(stdexec::detail::to_type_fold(
                           stdexec::types<int_prop, string_prop, double_prop>{})
                           .type(stdexec::detail::cw<1>))::type,
              string_prop>);
static_assert(std::same_as<
              decltype(stdexec::detail::to_type_fold(
                           stdexec::types<int_prop, string_prop, double_prop>{})
                           .type(stdexec::detail::cw<2>))::type,
              double_prop>);

#if HAVE_BOOST_MP11
using boost::mp11::mp_list;
#endif

TEST(env_, concept_)
{
    static_assert(stdexec::environment<decltype(stdexec::empty_env)>);

    stdexec::env env1 = stdexec::make_env(int_prop{}, 42)(double_prop{}, 13.0)(
        string_prop{}, std::string("foo"));

    static_assert(stdexec::environment<decltype(env1)>);
    static_assert(stdexec::environment<decltype(env1) const>);

    auto env2 = stdexec::env(
        stdexec::types<int_prop, double_prop, string_prop>{},
        std::tuple(42, 13.0, std::string("foo")));

    static_assert(stdexec::environment<decltype(env2)>);
    static_assert(stdexec::environment<decltype(env2) const>);

#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        static_assert(stdexec::environment<decltype(env1)>);
        static_assert(stdexec::environment<decltype(env1) const>);
    }
#endif
}

TEST(env_, make_env_)
{
    {
        stdexec::env env =
            stdexec::make_env(int_prop{}, 42)(double_prop{}, 13.0);

        auto const expected = stdexec::env(
            stdexec::types<int_prop, double_prop>{}, std::tuple(42, 13.0));

        EXPECT_TRUE(env == expected);
    }
    {
        stdexec::env env = stdexec::make_env(int_prop{}, 42) //
            (double_prop{}, 13.0)                            //
            (string_prop{}, std::string("foo"))              //
            (int_2_prop{}, std::unique_ptr<int>());

        auto const expected = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop, int_2_prop>{},
            std::tuple(42, 13.0, std::string("foo"), std::unique_ptr<int>()));

        EXPECT_TRUE(env == expected);
    }
#if HAVE_BOOST_MP11
    {
        stdexec::env env = stdexec::make_env_with<mp_list>(int_prop{}, 42)(
            double_prop{}, 13.0);

        auto const expected = stdexec::env(
            mp_list<int_prop, double_prop>{}, std::tuple(42, 13.0));

        EXPECT_TRUE(env == expected);
    }
    {
        stdexec::env env = stdexec::make_env_with<mp_list>(int_prop{}, 42) //
            (double_prop{}, 13.0)                                          //
            (string_prop{}, std::string("foo"))                            //
            (int_2_prop{}, std::unique_ptr<int>());

        auto const expected = stdexec::env(
            mp_list<int_prop, double_prop, string_prop, int_2_prop>{},
            std::tuple(42, 13.0, std::string("foo"), std::unique_ptr<int>()));

        EXPECT_TRUE(env == expected);
    }
#endif
}

TEST(env_, type_get)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const & c_env = env;

            EXPECT_EQ(stdexec::get<int_prop>(c_env), 42);
            EXPECT_EQ(stdexec::get<double_prop>(c_env), 13.0);
            EXPECT_EQ(stdexec::get<string_prop>(c_env), "foo");

            static_assert(std::same_as<
                          decltype(stdexec::get<int_prop>(c_env)),
                          int const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_prop>(c_env)),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<int_prop>(std::move(c_env))),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_prop>(std::move(c_env))),
                          std::string const &&>);
        }

        EXPECT_EQ(stdexec::get<int_prop>(env), 42);
        EXPECT_EQ(stdexec::get<double_prop>(env), 13.0);
        EXPECT_EQ(stdexec::get<string_prop>(env), "foo");

        stdexec::get<int_prop>(env) = 8;
        EXPECT_EQ(stdexec::get<int_prop>(env), 8);

        stdexec::get<double_prop>(env) = 19.9;
        EXPECT_EQ(stdexec::get<double_prop>(env), 19.9);

        std::string const moved_to = stdexec::get<string_prop>(std::move(env));
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(stdexec::get<string_prop>(env), "");

        static_assert(
            std::same_as<decltype(stdexec::get<int_prop>(env)), int &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_prop>(env)),
                      std::string &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<int_prop>(std::move(env))),
                      int &&>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_prop>(std::move(env))),
                      std::string &&>);
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const & c_env = env;

            EXPECT_EQ(stdexec::get<int_prop>(c_env), 42);
            EXPECT_EQ(stdexec::get<double_prop>(c_env), 13.0);
            EXPECT_EQ(stdexec::get<string_prop>(c_env), "foo");

            static_assert(std::same_as<
                          decltype(stdexec::get<int_prop>(c_env)),
                          int const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_prop>(c_env)),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<int_prop>(std::move(c_env))),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_prop>(std::move(c_env))),
                          std::string const &&>);
        }

        EXPECT_EQ(stdexec::get<int_prop>(env), 42);
        EXPECT_EQ(stdexec::get<double_prop>(env), 13.0);
        EXPECT_EQ(stdexec::get<string_prop>(env), "foo");

        stdexec::get<int_prop>(env) = 8;
        EXPECT_EQ(stdexec::get<int_prop>(env), 8);

        stdexec::get<double_prop>(env) = 19.9;
        EXPECT_EQ(stdexec::get<double_prop>(env), 19.9);

        std::string const moved_to = stdexec::get<string_prop>(std::move(env));
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(stdexec::get<string_prop>(env), "");

        static_assert(
            std::same_as<decltype(stdexec::get<int_prop>(env)), int &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_prop>(env)),
                      std::string &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<int_prop>(std::move(env))),
                      int &&>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_prop>(std::move(env))),
                      std::string &&>);
    }
#endif
}

#if USE_AUTO_INVOKE
TEST(env_, invocable_get)
{
    auto nullary = []() { return 77; };
    auto unary = [](unary_func_prop) { return 88; };

    {
        auto wrong_unary = [](int_prop) { return 99; };

        auto binary_impl = [](int a, int b) { return a + b; };
        std::function binary = binary_impl;

        auto env = stdexec::env(
            stdexec::types<
                int_prop,
                nullary_func_prop,
                unary_func_prop,
                binary_func_prop,
                int_2_prop>{},
            std::tuple(42, nullary, unary, binary, wrong_unary));

        EXPECT_EQ(stdexec::get<int_prop>(env), 42);
        EXPECT_EQ(stdexec::get<nullary_func_prop>(env), 77);
        EXPECT_EQ(stdexec::get<unary_func_prop>(env), 88);
        EXPECT_TRUE(
            stdexec::get<binary_func_prop>(env).target_type() ==
            binary.target_type());
        static_assert(
            std::invocable<decltype(stdexec::get<int_2_prop>(env)), int_prop>);
    }
#if USE_DONT_INVOKE
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, nullary_func_prop, unary_func_prop>{},
            std::tuple(
                42,
                stdexec::dont_invoke{nullary},
                stdexec::dont_invoke{unary}));

        static_assert(
            std::invocable<decltype(stdexec::get<nullary_func_prop>(env))>);
        static_assert(std::invocable<
                      decltype(stdexec::get<unary_func_prop>(env)),
                      unary_func_prop>);
    }
#endif
}
#endif

TEST(env_, nttp_get)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const & c_env = env;

            EXPECT_EQ(stdexec::get<0>(env), 42);
            EXPECT_EQ(stdexec::get<1>(env), 13.0);
            EXPECT_EQ(stdexec::get<2>(env), "foo");

            static_assert(
                std::same_as<decltype(stdexec::get<0>(c_env)), int const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<2>(c_env)),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<0>(std::move(c_env))),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(stdexec::get<2>(std::move(c_env))),
                          std::string const &&>);
        }

        EXPECT_EQ(stdexec::get<0>(env), 42);
        EXPECT_EQ(stdexec::get<1>(env), 13.0);
        EXPECT_EQ(stdexec::get<2>(env), "foo");

        stdexec::get<0>(env) = 8;
        EXPECT_EQ(stdexec::get<0>(env), 8);

        stdexec::get<1>(env) = 19.9;
        EXPECT_EQ(stdexec::get<1>(env), 19.9);

        std::string const moved_to = stdexec::get<2>(std::move(env));
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(stdexec::get<2>(env), "");

        static_assert(std::same_as<decltype(stdexec::get<0>(env)), int &>);
        static_assert(
            std::same_as<decltype(stdexec::get<2>(env)), std::string &>);
        static_assert(
            std::same_as<decltype(stdexec::get<0>(std::move(env))), int &&>);
        static_assert(std::same_as<
                      decltype(stdexec::get<2>(std::move(env))),
                      std::string &&>);
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const & c_env = env;

            EXPECT_EQ(stdexec::get<0>(env), 42);
            EXPECT_EQ(stdexec::get<1>(env), 13.0);
            EXPECT_EQ(stdexec::get<2>(env), "foo");

            static_assert(
                std::same_as<decltype(stdexec::get<0>(c_env)), int const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<2>(c_env)),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<0>(std::move(c_env))),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(stdexec::get<2>(std::move(c_env))),
                          std::string const &&>);
        }

        EXPECT_EQ(stdexec::get<0>(env), 42);
        EXPECT_EQ(stdexec::get<1>(env), 13.0);
        EXPECT_EQ(stdexec::get<2>(env), "foo");

        stdexec::get<0>(env) = 8;
        EXPECT_EQ(stdexec::get<0>(env), 8);

        stdexec::get<1>(env) = 19.9;
        EXPECT_EQ(stdexec::get<1>(env), 19.9);

        std::string const moved_to = stdexec::get<2>(std::move(env));
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(stdexec::get<2>(env), "");

        static_assert(std::same_as<decltype(stdexec::get<0>(env)), int &>);
        static_assert(
            std::same_as<decltype(stdexec::get<2>(env)), std::string &>);
        static_assert(
            std::same_as<decltype(stdexec::get<0>(std::move(env))), int &&>);
        static_assert(std::same_as<
                      decltype(stdexec::get<2>(std::move(env))),
                      std::string &&>);
    }
#endif
}

TEST(env_, detail_prop_to_value)
{
    std::array<std::string, 4> strings_arr = {"a", "b", "c", "d"};
    std::vector<std::string> strings_vec(
        strings_arr.begin(), strings_arr.end());
    std::vector<std::string_view> string_views = {
        strings_vec[0], strings_vec[1], strings_vec[2], strings_vec[3]};
    std::vector<std::reference_wrapper<std::string_view>> string_refs_vec = {
        std::ref(string_views[0]),
        std::ref(string_views[1]),
        std::ref(string_views[2]),
        std::ref(string_views[3])};
    std::array<std::reference_wrapper<std::string_view>, 4> string_refs_arr = {
        std::ref(string_views[0]),
        std::ref(string_views[1]),
        std::ref(string_views[2]),
        std::ref(string_views[3])};

    EXPECT_EQ(stdexec::detail::prop_to_value(strings_arr), strings_arr);
    EXPECT_EQ(stdexec::detail::prop_to_value(strings_vec), strings_vec);
    EXPECT_EQ(stdexec::detail::prop_to_value(string_views), strings_vec);
    EXPECT_EQ(stdexec::detail::prop_to_value(string_refs_vec), strings_vec);
    EXPECT_EQ(stdexec::detail::prop_to_value(string_refs_arr), strings_arr);

    std::span<std::reference_wrapper<std::string_view>, 4> fixed_span(
        string_refs_vec);
    std::span<std::reference_wrapper<std::string_view>> span(string_refs_vec);

    EXPECT_EQ(stdexec::detail::prop_to_value(fixed_span), strings_arr);
    EXPECT_EQ(stdexec::detail::prop_to_value(span), strings_vec);
}

TEST(env_, index_contains_equals)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        EXPECT_TRUE(env == env);

        {
            auto const & c_env = env;

            EXPECT_TRUE(env == c_env);

            EXPECT_EQ(stdexec::get<int_prop>(env), 42);
            EXPECT_EQ(stdexec::get<double_prop>(env), 13.0);
            EXPECT_EQ(stdexec::get<string_prop>(env), "foo");

            static_assert(
                std::same_as<decltype(c_env[int_prop{}]), int const &>);
            static_assert(std::same_as<
                          decltype(c_env[string_prop{}]),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[int_prop{}]),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[string_prop{}]),
                          std::string const &&>);
        }

        EXPECT_TRUE(stdexec::contains(env, int_prop{}));
        EXPECT_TRUE(stdexec::contains(env, double_prop{}));
        EXPECT_TRUE(stdexec::contains(env, string_prop{}));
        EXPECT_FALSE(stdexec::contains(env, int{}));

        EXPECT_TRUE(stdexec::contains_all_of(env, stdexec::types<int_prop>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<double_prop>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<string_prop>{}));

        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_prop, int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<double_prop, int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_prop, double_prop, int_prop>{}));

        EXPECT_FALSE(
            stdexec::contains_all_of(env, stdexec::types<int_2_prop>{}));
        EXPECT_FALSE(stdexec::contains_all_of(
            env, stdexec::types<int_prop, int_2_prop>{}));

#if HAVE_BOOST_MP11
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<double_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<string_prop>{}));

        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<string_prop, int_prop>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<double_prop, int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, mp_list<string_prop, double_prop, int_prop>{}));

        EXPECT_FALSE(stdexec::contains_all_of(env, mp_list<int_2_prop>{}));
        EXPECT_FALSE(
            stdexec::contains_all_of(env, mp_list<int_prop, int_2_prop>{}));
#endif

        EXPECT_EQ(env[int_prop{}], 42);
        EXPECT_EQ(env[double_prop{}], 13.0);
        EXPECT_EQ(env[string_prop{}], "foo");

        env[int_prop{}] = 8;
        EXPECT_EQ(env[int_prop{}], 8);

        env[double_prop{}] = 19.9;
        EXPECT_EQ(env[double_prop{}], 19.9);

        std::string const moved_to = std::move(env)[string_prop{}];
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(env[string_prop{}], "");

        static_assert(std::same_as<decltype(env[int_prop{}]), int &>);
        static_assert(
            std::same_as<decltype(env[string_prop{}]), std::string &>);
        static_assert(
            std::same_as<decltype(std::move(env)[int_prop{}]), int &&>);
        static_assert(std::same_as<
                      decltype(std::move(env)[string_prop{}]),
                      std::string &&>);
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        EXPECT_TRUE(env == env);

        {
            auto const & c_env = env;

            EXPECT_TRUE(env == c_env);

            EXPECT_EQ(stdexec::get<int_prop>(env), 42);
            EXPECT_EQ(stdexec::get<double_prop>(env), 13.0);
            EXPECT_EQ(stdexec::get<string_prop>(env), "foo");

            static_assert(
                std::same_as<decltype(c_env[int_prop{}]), int const &>);
            static_assert(std::same_as<
                          decltype(c_env[string_prop{}]),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[int_prop{}]),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[string_prop{}]),
                          std::string const &&>);
        }

        EXPECT_TRUE(stdexec::contains(env, int_prop{}));
        EXPECT_TRUE(stdexec::contains(env, double_prop{}));
        EXPECT_TRUE(stdexec::contains(env, string_prop{}));
        EXPECT_FALSE(stdexec::contains(env, int{}));

        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<double_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<string_prop>{}));

        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<string_prop, int_prop>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<double_prop, int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, mp_list<string_prop, double_prop, int_prop>{}));

        EXPECT_FALSE(stdexec::contains_all_of(env, mp_list<int_2_prop>{}));
        EXPECT_FALSE(
            stdexec::contains_all_of(env, mp_list<int_prop, int_2_prop>{}));

        EXPECT_TRUE(stdexec::contains_all_of(env, stdexec::types<int_prop>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<double_prop>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<string_prop>{}));

        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_prop, int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<double_prop, int_prop>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_prop, double_prop, int_prop>{}));

        EXPECT_FALSE(
            stdexec::contains_all_of(env, stdexec::types<int_2_prop>{}));
        EXPECT_FALSE(stdexec::contains_all_of(
            env, stdexec::types<int_prop, int_2_prop>{}));

        EXPECT_EQ(env[int_prop{}], 42);
        EXPECT_EQ(env[double_prop{}], 13.0);
        EXPECT_EQ(env[string_prop{}], "foo");

        env[int_prop{}] = 8;
        EXPECT_EQ(env[int_prop{}], 8);

        env[double_prop{}] = 19.9;
        EXPECT_EQ(env[double_prop{}], 19.9);

        std::string const moved_to = std::move(env)[string_prop{}];
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(env[string_prop{}], "");

        static_assert(std::same_as<decltype(env[int_prop{}]), int &>);
        static_assert(
            std::same_as<decltype(env[string_prop{}]), std::string &>);
        static_assert(
            std::same_as<decltype(std::move(env)[int_prop{}]), int &&>);
        static_assert(std::same_as<
                      decltype(std::move(env)[string_prop{}]),
                      std::string &&>);
    }
#endif
}

TEST(env_, single_insert)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, double_prop, string_prop>{},
                std::tuple(42, 13.0, std::string("foo")));
            auto inserted =
                stdexec::insert(env, string_prop{}, std::string("foo"));
            EXPECT_TRUE(inserted == expected);

            auto const expected_2 = stdexec::env(
                stdexec::
                    types<int_prop, double_prop, string_prop, string_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), std::string("bar")));

            auto inserted_2 = stdexec::insert(
                std::move(inserted), string_2_prop{}, std::string("bar"));
            EXPECT_TRUE(inserted_2 == expected_2);

            auto const inserted_expected_after_move = stdexec::env(
                stdexec::types<int_prop, double_prop, string_prop>{},
                std::tuple(42, 13.0, std::string()));
            EXPECT_TRUE(inserted == inserted_expected_after_move);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_prop, double_prop>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                mp_list<int_prop, double_prop, string_prop>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto inserted =
                stdexec::insert(env, string_prop{}, std::string("foo"));
            EXPECT_TRUE(inserted == expected);

            auto const expected_2 = stdexec::env(
                mp_list<int_prop, double_prop, string_prop, string_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), std::string("bar")));

            auto inserted_2 = stdexec::insert(
                std::move(inserted), string_2_prop{}, std::string("bar"));
            EXPECT_TRUE(inserted_2 == expected_2);

            auto const inserted_expected_after_move = stdexec::env(
                mp_list<int_prop, double_prop, string_prop>{},
                std::tuple(42, 13.0, std::string()));
            EXPECT_TRUE(inserted == inserted_expected_after_move);
        }
    }
#endif
}

TEST(env_, slice)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto slice = stdexec::slice(
                env, stdexec::types<int_prop, double_prop, string_prop>{});
            EXPECT_TRUE(slice == env);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto slice = stdexec::slice(
                env, stdexec::types<int_prop, string_prop, double_prop>{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<string_prop, double_prop>{},
                std::tuple(std::string("foo"), 13.0));

            auto slice = stdexec::slice(env, string_prop{}, double_prop{});
            EXPECT_TRUE(slice == expected);
        }
#if HAVE_BOOST_MP11
        {
            auto const expected = stdexec::env(
                mp_list<int_prop, double_prop, string_prop>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto slice = stdexec::slice(
                env, mp_list<int_prop, double_prop, string_prop>{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto slice = stdexec::slice(
                env, mp_list<int_prop, string_prop, double_prop>{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<string_prop, double_prop>{},
                std::tuple(std::string("foo"), 13.0));

            auto slice = stdexec::slice(env, string_prop{}, double_prop{});
            EXPECT_TRUE(slice == expected);
        }
#endif
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, double_prop, string_prop>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto slice = stdexec::slice(
                env, stdexec::types<int_prop, double_prop, string_prop>{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto slice = stdexec::slice(
                env, stdexec::types<int_prop, string_prop, double_prop>{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<string_prop, double_prop>{},
                std::tuple(std::string("foo"), 13.0));

            auto slice = stdexec::slice(env, string_prop{}, double_prop{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_prop, double_prop, string_prop>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto slice = stdexec::slice(
                env, mp_list<int_prop, double_prop, string_prop>{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto slice = stdexec::slice(
                env, mp_list<int_prop, string_prop, double_prop>{});
            EXPECT_TRUE(slice == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<string_prop, double_prop>{},
                std::tuple(std::string("foo"), 13.0));

            auto slice = stdexec::slice(env, string_prop{}, double_prop{});
            EXPECT_TRUE(slice == expected);
        }
    }
#endif
}

TEST(env_, insert_env)
{
    {
        auto const env1 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == env2);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto const env1 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, double_prop, string_prop>{},
                std::tuple(120, 19.9, std::string("bar")));

            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                mp_list<int_prop, double_prop, string_prop>{},
                std::tuple(120, 19.9, std::string("bar")));

            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == env2);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_prop, string_prop, double_prop>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
#endif
}

TEST(env_, insert_unique)
{
    {
        auto const env1 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert_unique(env1, env2);
            EXPECT_TRUE(result == env1);
        }
        {
            auto result = stdexec::insert_unique(env1, env1_less);
            EXPECT_TRUE(result == env1);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert_unique(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto const env1 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert_unique(env1, env2);
            EXPECT_TRUE(result == env1);
        }
        {
            auto result = stdexec::insert_unique(env1, env1_less);
            EXPECT_TRUE(result == env1);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert_unique(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert_unique(env1, env2);
            EXPECT_TRUE(result == env1);
        }
        {
            auto result = stdexec::insert_unique(env1, env1_less);
            EXPECT_TRUE(result == env1);
        }
        {
            auto const expected = stdexec::env(
                mp_list<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert_unique(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_prop, double_prop>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_prop, double_2_prop>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert_unique(env1, env2);
            EXPECT_TRUE(result == env1);
        }
        {
            auto result = stdexec::insert_unique(env1, env1_less);
            EXPECT_TRUE(result == env1);
        }
        {
            auto const expected = stdexec::env(
                mp_list<
                    int_prop,
                    double_prop,
                    string_prop,
                    int_2_prop,
                    double_2_prop>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert_unique(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
#endif
}

TEST(env_, single_erase)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, double_prop>{}, std::tuple(42, 13.0));

            auto erased = stdexec::erase(env, string_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                stdexec::types<int_prop, string_prop>{},
                std::tuple(42, std::string("foo")));

            auto erased = stdexec::erase(env, double_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                stdexec::types<double_prop, string_prop>{},
                std::tuple(13.0, std::string("foo")));

            auto erased = stdexec::erase(env, int_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto erased_1 = stdexec::erase(env, int_prop{});
            auto erased_2 = stdexec::erase(erased_1, double_prop{});
            auto final_ = stdexec::erase(erased_2, string_prop{});
            EXPECT_TRUE(final_ == stdexec::empty_env);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const expected = stdexec::env(
                mp_list<int_prop, double_prop>{}, std::tuple(42, 13.0));

            auto erased = stdexec::erase(env, string_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                mp_list<int_prop, string_prop>{},
                std::tuple(42, std::string("foo")));

            auto erased = stdexec::erase(env, double_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                mp_list<double_prop, string_prop>{},
                std::tuple(13.0, std::string("foo")));

            auto erased = stdexec::erase(env, int_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto erased_1 = stdexec::erase(env, int_prop{});
            auto erased_2 = stdexec::erase(erased_1, double_prop{});
            auto final_ = stdexec::erase(erased_2, string_prop{});
            EXPECT_TRUE(final_ == (stdexec::env<mp_list<>, std::tuple<>>()));
        }
    }
#endif
}

TEST(env_, multi_erase)
{
    {
        auto const initial_env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                stdexec::types<string_prop>{}, std::tuple(std::string("foo")));

            auto erased = stdexec::erase(env, int_prop{}, double_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                stdexec::types<string_prop>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(std::move(env), double_prop{}, int_prop{});
            EXPECT_TRUE(erased == expected);

            EXPECT_EQ(env[string_prop{}], std::string());
        }

        {
            auto env = initial_env;

            auto const expected =
                stdexec::env(stdexec::types<double_prop>{}, std::tuple(13.0));

            auto erased = stdexec::erase(env, int_prop{}, string_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                stdexec::types<string_prop>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(env, string_prop{}, double_prop{}, int_prop{});
            EXPECT_TRUE(erased == stdexec::empty_env);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto const initial_env = stdexec::env(
            mp_list<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                mp_list<string_prop>{}, std::tuple(std::string("foo")));

            auto erased = stdexec::erase(env, int_prop{}, double_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                mp_list<string_prop>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(std::move(env), double_prop{}, int_prop{});
            EXPECT_TRUE(erased == expected);

            EXPECT_EQ(env[string_prop{}], std::string());
        }

        {
            auto env = initial_env;

            auto const expected =
                stdexec::env(mp_list<double_prop>{}, std::tuple(13.0));

            auto erased = stdexec::erase(env, int_prop{}, string_prop{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                mp_list<string_prop>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(env, string_prop{}, double_prop{}, int_prop{});
            EXPECT_TRUE(erased == (stdexec::env<mp_list<>, std::tuple<>>()));
        }
    }
#endif
}

TEST(env_, deep_copy_)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        auto copy = stdexec::deep_copy(env);

        EXPECT_TRUE(copy == env);
    }
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string_view("foo")));

        static_assert(
            std::same_as<
                decltype(env),
                stdexec::env<
                    stdexec::types<int_prop, double_prop, string_prop>,
                    std::tuple<
                        int,
                        double,
                        std::
                            basic_string_view<char, std::char_traits<char>>>>>);

        auto copy = stdexec::deep_copy(env);

        static_assert(
            std::same_as<
                decltype(copy),
                stdexec::env<
                    stdexec::types<int_prop, double_prop, string_prop>,
                    std::tuple<
                        int,
                        double,
                        std::basic_string<char, std::char_traits<char>>>>>);

        EXPECT_EQ(copy[int_prop{}], env[int_prop{}]);
        EXPECT_EQ(copy[double_prop{}], env[double_prop{}]);
        EXPECT_EQ(copy[string_prop{}], env[string_prop{}]);

        EXPECT_TRUE(copy == env);
    }
    {
        auto initial_env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string_view("foo")));

        auto env = stdexec::with_only(
            std::ref(initial_env), stdexec::types<string_prop, int_prop>{});

        auto const expected = stdexec::env(
            stdexec::types<string_prop, int_prop>{},
            std::tuple(std::string("foo"), 42));

        auto copy = stdexec::deep_copy(env);

        EXPECT_TRUE(copy == expected);
    }
}

TEST(env_, ref_env_)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        stdexec::ref_env ref1 = env;
        stdexec::ref_env ref2 = std::ref(env);

        EXPECT_EQ(ref1[int_prop{}], env[int_prop{}]);
        EXPECT_EQ(ref1[double_prop{}], env[double_prop{}]);
        EXPECT_EQ(ref1[string_prop{}], env[string_prop{}]);

        EXPECT_EQ(ref1[int_prop{}], ref2[int_prop{}]);
        EXPECT_EQ(ref1[double_prop{}], ref2[double_prop{}]);
        EXPECT_EQ(ref1[string_prop{}], ref2[string_prop{}]);

        static_assert(stdexec::environment<decltype(ref1)>);
        static_assert(stdexec::environment<decltype(ref1) &>);
        static_assert(stdexec::environment<decltype(ref1) const &>);
        static_assert(stdexec::environment<decltype(ref1) &&>);
        static_assert(stdexec::environment<decltype(ref1) const &&>);

        {
            auto const & c_env = env;

            stdexec::ref_env ref1 = c_env;
            stdexec::ref_env ref2 = std::ref(c_env);

            EXPECT_EQ(ref1[int_prop{}], env[int_prop{}]);
            EXPECT_EQ(ref1[double_prop{}], env[double_prop{}]);
            EXPECT_EQ(ref1[string_prop{}], env[string_prop{}]);

            EXPECT_EQ(ref1[int_prop{}], ref2[int_prop{}]);
            EXPECT_EQ(ref1[double_prop{}], ref2[double_prop{}]);
            EXPECT_EQ(ref1[string_prop{}], ref2[string_prop{}]);
        }
    }
}

TEST(env_, computed_env_)
{
    auto universal = [](auto) { return 42; };

    auto env = stdexec::computed_env(
        universal, stdexec::types<int_prop, double_prop, string_prop>{});

    static_assert(stdexec::contains_all_of<decltype(env)>(
        stdexec::properties_t<decltype(env)>{}));

    EXPECT_EQ(env[int_prop{}], universal(int_prop{}));
    EXPECT_EQ(env[double_prop{}], universal(double_prop{}));
    EXPECT_EQ(env[string_prop{}], universal(string_prop{}));

    static_assert(stdexec::environment<decltype(env)>);
    static_assert(stdexec::environment<decltype(env) &>);
    static_assert(stdexec::environment<decltype(env) const &>);
    static_assert(stdexec::environment<decltype(env) &&>);
    static_assert(stdexec::environment<decltype(env) const &&>);
}

TEST(env_, layer_env_)
{
    {
        auto int_env = stdexec::env(stdexec::types<int_prop>{}, std::tuple(42));

        auto double_env =
            stdexec::env(stdexec::types<double_prop>{}, std::tuple(13.0));

        {
            auto env = stdexec::layer_env(int_env, double_env);

            static_assert(
                std::same_as<
                    decltype(env),
                    stdexec::layer_env<
                        stdexec::env<stdexec::types<int_prop>, std::tuple<int>>,
                        stdexec::env<
                            stdexec::types<double_prop>,
                            std::tuple<double>>>>);

            EXPECT_EQ(env[int_prop{}], int_env[int_prop{}]);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
        }
        {
            auto env = int_env | stdexec::layer(double_env);
            static_assert(
                std::same_as<
                    decltype(env),
                    stdexec::layer_env<
                        stdexec::env<stdexec::types<int_prop>, std::tuple<int>>,
                        stdexec::env<
                            stdexec::types<double_prop>,
                            std::tuple<double>>>>);

            EXPECT_EQ(env[int_prop{}], int_env[int_prop{}]);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
        }
        {
            auto env = int_env | stdexec::layer(std::ref(double_env));
            static_assert(
                std::same_as<
                    decltype(env),
                    stdexec::layer_env<
                        stdexec::env<stdexec::types<int_prop>, std::tuple<int>>,
                        stdexec::ref_env<stdexec::env<
                            stdexec::types<double_prop>,
                            std::tuple<double>>>>>);

            EXPECT_EQ(env[int_prop{}], int_env[int_prop{}]);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
        }
        {
            auto env = std::ref(int_env) | stdexec::layer(double_env);
            static_assert(std::same_as<
                          decltype(env),
                          stdexec::layer_env<
                              stdexec::ref_env<stdexec::env<
                                  stdexec::types<int_prop>,
                                  std::tuple<int>>>,
                              stdexec::env<
                                  stdexec::types<double_prop>,
                                  std::tuple<double>>>>);

            EXPECT_EQ(env[int_prop{}], int_env[int_prop{}]);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
        }
        {
            auto env = std::ref(int_env) | stdexec::layer(std::ref(double_env));
            static_assert(std::same_as<
                          decltype(env),
                          stdexec::layer_env<
                              stdexec::ref_env<stdexec::env<
                                  stdexec::types<int_prop>,
                                  std::tuple<int>>>,
                              stdexec::ref_env<stdexec::env<
                                  stdexec::types<double_prop>,
                                  std::tuple<double>>>>>);

            EXPECT_EQ(env[int_prop{}], int_env[int_prop{}]);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
        }

        auto string_env = stdexec::env(
            stdexec::types<string_prop>{}, std::tuple(std::string("foo")));

        {
            auto env = int_env | stdexec::layer(double_env) |
                       stdexec::layer(string_env);

            EXPECT_EQ(env[int_prop{}], int_env[int_prop{}]);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
            EXPECT_EQ(env[string_prop{}], string_env[string_prop{}]);
        }

        {
            auto int_double_env = stdexec::env(
                stdexec::types<int_prop, double_prop>{}, std::tuple(55, 13.0));

            auto const & c_string_env = string_env;

            auto env = int_double_env | stdexec::layer(std::ref(double_env)) |
                       stdexec::layer(std::ref(c_string_env));

            EXPECT_EQ(env[int_prop{}], 55);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
            EXPECT_EQ(env[string_prop{}], string_env[string_prop{}]);

            static_assert(stdexec::environment<decltype(env)>);
            static_assert(stdexec::environment<decltype(env) &>);
            static_assert(stdexec::environment<decltype(env) const &>);
            static_assert(stdexec::environment<decltype(env) &&>);
            static_assert(stdexec::environment<decltype(env) const &&>);
        }

        {
            auto env = int_env | stdexec::layer(double_env) |
                       stdexec::layer(std::move(string_env));

            EXPECT_EQ(env[int_prop{}], int_env[int_prop{}]);
            EXPECT_EQ(env[double_prop{}], double_env[double_prop{}]);
            EXPECT_EQ(env[string_prop{}], "foo");
            EXPECT_EQ(string_env[string_prop{}], "");
        }
    }
}

TEST(env_, with_only_env_)
{
    {
        auto const initial_env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env = stdexec::with_only(
                initial_env, stdexec::types<string_prop, int_prop>{});

            static_assert(!stdexec::contains(env, double_prop{}));
            EXPECT_EQ(env[int_prop{}], initial_env[int_prop{}]);
            EXPECT_EQ(env[string_prop{}], initial_env[string_prop{}]);

            static_assert(stdexec::environment<decltype(env)>);
            static_assert(stdexec::environment<decltype(env) &>);
            static_assert(stdexec::environment<decltype(env) const &>);
            static_assert(stdexec::environment<decltype(env) &&>);
            static_assert(stdexec::environment<decltype(env) const &&>);
        }
    }
}

TEST(env_, without_env_)
{
    {
        auto const initial_env = stdexec::env(
            stdexec::types<int_prop, double_prop, string_prop>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env =
                initial_env | stdexec::without(stdexec::types<double_prop>{});

            static_assert(!stdexec::contains(env, double_prop{}));
            EXPECT_EQ(env[int_prop{}], initial_env[int_prop{}]);
            EXPECT_EQ(env[string_prop{}], initial_env[string_prop{}]);

            static_assert(stdexec::environment<decltype(env)>);
            static_assert(stdexec::environment<decltype(env) &>);
            static_assert(stdexec::environment<decltype(env) const &>);
            static_assert(stdexec::environment<decltype(env) &&>);
            static_assert(stdexec::environment<decltype(env) const &&>);
        }
    }
}

#endif
