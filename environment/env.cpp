#include <ranges>
#include <tuple>

#ifndef DO_TESTING
#define DO_TESTING 1
#endif

#if DO_TESTING
#include <gtest/gtest.h>
#include <string>

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
            typename Tag,
            typename T,
            int I = 0,
            typename Tail = temp_fold_base<TypeList>>
        struct temp_fold;
        template<template<class...> class TypeList>
        struct temp_fold_base
        {
            consteval void tag_type() const {}
            constexpr void value() && {}
            template<typename T>
            static consteval bool contains(wrapper<T>)
            {
                return false;
            }

            template<typename Tag, typename T>
            constexpr auto operator()(Tag, T && x)
            {
                return temp_fold<TypeList, Tag, T>{
                    std::move(*this), std::move(x)};
            }
        };

        template<typename TempFolded, typename Tag>
        concept new_tag = (!TempFolded::contains(detail::wrap<Tag>()));

        template<
            template<class...>
            class TypeList,
            typename Tag,
            typename T,
            int I,
            typename Tail>
        struct temp_fold : Tail
        {
            temp_fold(Tail && tail, T && x) :
                Tail(std::move(tail)), x_(std::move(x))
            {}
            consteval wrapper<Tag> tag_type(constant_wrapper<I>) const
            {
                return {};
            }
            constexpr T && value(constant_wrapper<I>) &&
            {
                return std::move(x_);
            }
            static consteval bool contains(wrapper<Tag>) { return true; }

            template<typename Tag2, typename T2>
            constexpr auto operator()(Tag2, T2 && x)
            requires new_tag<temp_fold, Tag2>
            {
                return temp_fold<TypeList, Tag2, T2, I + 1, temp_fold>{
                    {std::move(*this)}, std::move(x)};
            }

            using Tail::tag_type;
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
            typename Tag,
            template<typename...>
            typename TypeList,
            typename T,
            typename... Ts>
        consteval int index_from_tag(TypeList<T, Ts...> tl)
        {
            return detail::to_type_fold(tl).index(detail::wrap<Tag>());
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
    concept queryable_with = requires (T env, U tag) {
        { env[tag] } -> detail::non_void;
    };
    // clang-format on
    namespace detail {
        template<typename T, typename U>
        constexpr bool queryable_with_all = false;
        template<typename T, template<class...> class TypeList, typename... Ts>
        constexpr bool queryable_with_all<T, TypeList<Ts...>> =
            (queryable_with<T, Ts> && ...);
    }
    // clang-format off
    template<typename T>
    concept environment =
        requires(T x) { typename T::tags_type; } &&
        type_list<typename T::tags_type> &&
        detail::queryable_with_all<T, typename T::tags_type>;
    // clang-format on

    namespace detail {
        template<typename Tag, typename Tags>
        constexpr bool has_type = 0 <= detail::index_from_tag<Tag>(Tags{});

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
            typename... Tags2,
            typename... UniqueTags>
        consteval auto tl_set_diff_impl(
            TypeList<> tl1,
            TypeList<Tags2...> tl2,
            TypeList<UniqueTags...> result)
        {
            return result;
        }
        template<
            template<class...>
            class TypeList,
            typename Tag,
            typename... Tags1,
            typename... Tags2,
            typename... UniqueTags>
        consteval auto tl_set_diff_impl(
            TypeList<Tag, Tags1...> tl1,
            TypeList<Tags2...> tl2,
            TypeList<UniqueTags...> result)
        {
            constexpr int i = index_from_tag<Tag>(tl2);
            if constexpr (0 <= i) {
                return detail::tl_set_diff_impl(
                    TypeList<Tags1...>{}, tl2, TypeList<UniqueTags...>{});
            } else {
                return detail::tl_set_diff_impl(
                    TypeList<Tags1...>{}, tl2, TypeList<UniqueTags..., Tag>{});
            }
        }
        template<
            template<class...>
            class TypeList1,
            typename... Tags1,
            template<class...>
            class TypeList2,
            typename... Tags2>
        consteval auto
        tl_set_diff(TypeList1<Tags1...> tl1, TypeList2<Tags2...> tl2)
        {
            return detail::tl_set_diff_impl(
                tl1, TypeList1<Tags2...>{}, TypeList1<>{});
        }

        template<
            typename... Ts,
            template<class...>
            class TypeList,
            typename... Tags>
        constexpr bool
        same_arity_impl(TypeList<Tags...> tl, wrapper<std::tuple<Ts...>> w)
        {
            return sizeof...(Ts) == sizeof...(Tags);
        }
        template<typename Tags, typename Tuple>
        constexpr bool
            same_arity = detail::same_arity_impl(Tags{}, detail::wrap<Tuple>());

        template<typename T>
        constexpr bool is_tuple_v = false;
        template<typename... Ts>
        constexpr bool is_tuple_v<std::tuple<Ts...>> = true;
        template<typename T>
        concept is_tuple = is_tuple_v<T>;
    }

    template<typename T, typename Types>
    concept in_type_list = detail::has_type<T, Types>;

    template<typename... Ts>
    struct types
    {};

    template<type_list Tags, typename Tuple>
    requires detail::same_arity<Tags, Tuple> && detail::is_tuple<Tuple>
    struct env;

    namespace detail {
        template<
            template<class...>
            class TypeList,
            typename Tag,
            typename T,
            int I,
            typename Tail,
            int... Is>
        auto temp_fold_tags(
            temp_fold<TypeList, Tag, T, I, Tail> const & folded,
            std::integer_sequence<int, Is...>)
        {
            return TypeList<typename decltype(folded.tag_type(
                cw<Is>))::type...>{};
        }

        template<
            template<class...>
            class TypeList,
            typename Tag,
            typename T,
            int I,
            typename Tail,
            int... Is>
        auto temp_fold_values(
            temp_fold<TypeList, Tag, T, I, Tail> && folded,
            std::integer_sequence<int, Is...>)
        {
            return std::tuple(std::move(folded).value(cw<Is>)...);
        }
    }

    template<typename T>
    struct dont_invoke
    {
        T value;
    };

    namespace detail {
        template<typename T>
        constexpr bool is_dont_invoke = false;
        template<typename T>
        constexpr bool is_dont_invoke<dont_invoke<T>> = true;

        template<typename T>
        concept nonvoid_nullary_invocable = std::invocable<T> &&
            (!std::is_void_v<std::invoke_result_t<T>>);
        template<typename T, typename Tag>
        concept nonvoid_tag_invocable = std::invocable<T, Tag> &&
            (!std::is_void_v<std::invoke_result_t<T, Tag>>);
    }

    template<type_list Tags, typename Tuple>
    requires detail::same_arity<Tags, Tuple> && detail::is_tuple<Tuple>
    struct env
    {
        using tags_type = Tags;
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

        constexpr env(Tags tags, Tuple const & values) :
            tags(tags), values(values)
        {}
        constexpr env(Tags tags, Tuple && values) :
            tags(tags), values(std::move(values))
        {}

        template<
            template<class...>
            class TypeList,
            typename Tag,
            typename T,
            int I,
            typename Tail>
        constexpr env(detail::temp_fold<TypeList, Tag, T, I, Tail> && folded) :
            tags(detail::temp_fold_tags(
                folded, std::make_integer_sequence<int, I + 1>{})),
            values(detail::temp_fold_values(
                std::move(folded), std::make_integer_sequence<int, I + 1>{}))
        {}

        constexpr bool operator==(env const & other) const
        {
            return values == other.values;
        }

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag t)
        {
            constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
            using result_type = std::remove_cvref_t<decltype(std::get<i>(
                ((Self &&) self).values))>;
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(((Self &&) self).values).value;
            } else if constexpr (detail::nonvoid_nullary_invocable<
                                     result_type>) {
                return std::get<i>(((Self &&) self).values)();
            } else if constexpr (detail::
                                     nonvoid_tag_invocable<result_type, Tag>) {
                return std::get<i>(((Self &&) self).values)(t);
            } else {
                return std::get<i>(((Self &&) self).values);
            }
        }
#else
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag t) &
        {
            constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(values))>;
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(values).value;
            } else if constexpr (detail::nonvoid_nullary_invocable<
                                     result_type>) {
                return std::get<i>(values)();
            } else if constexpr (detail::
                                     nonvoid_tag_invocable<result_type, Tag>) {
                return std::get<i>(values)(t);
            } else {
                return std::get<i>(values);
            }
        }
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag t) const &
        {
            constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(values))>;
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(values).value;
            } else if constexpr (detail::nonvoid_nullary_invocable<
                                     result_type>) {
                return std::get<i>(values)();
            } else if constexpr (detail::
                                     nonvoid_tag_invocable<result_type, Tag>) {
                return std::get<i>(values)(t);
            } else {
                return std::get<i>(values);
            }
        }
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag t) &&
        {
            constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(std::move(values)))>;
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(std::move(values)).value;
            } else if constexpr (detail::nonvoid_nullary_invocable<
                                     result_type>) {
                return std::get<i>(std::move(values))();
            } else if constexpr (detail::
                                     nonvoid_tag_invocable<result_type, Tag>) {
                return std::get<i>(std::move(values))(t);
            } else {
                return std::get<i>(std::move(values));
            }
        }
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag t) const &&
        {
            constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
            using result_type =
                std::remove_cvref_t<decltype(std::get<i>(std::move(values)))>;
            if constexpr (detail::is_dont_invoke<result_type>) {
                return std::get<i>(std::move(values)).value;
            } else if constexpr (detail::nonvoid_nullary_invocable<
                                     result_type>) {
                return std::get<i>(std::move(values))();
            } else if constexpr (detail::
                                     nonvoid_tag_invocable<result_type, Tag>) {
                return std::get<i>(std::move(values))(t);
            } else {
                return std::get<i>(std::move(values));
            }
        }
#endif

        [[no_unique_address]] Tags tags;
        Tuple values;
    };

    template<
        template<class...>
        class TypeList,
        typename Tag,
        typename T,
        int I,
        typename Tail>
    env(detail::temp_fold<TypeList, Tag, T, I, Tail> && folded) -> env<
        decltype(detail::temp_fold_tags(
            std::declval<detail::temp_fold<TypeList, Tag, T, I, Tail>>(),
            std::make_integer_sequence<int, I + 1>{})),
        decltype(detail::temp_fold_values(
            std::declval<detail::temp_fold<TypeList, Tag, T, I, Tail>>(),
            std::make_integer_sequence<int, I + 1>{}))>;

    inline constexpr env<types<>, std::tuple<>> empty_env;

    inline detail::temp_fold_base<types> make_env;

    template<template<class...> class TypeList>
    detail::temp_fold_base<TypeList> make_env_with;

    template<size_t I, typename Tags, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> & env)
    {
        return std::get<I>(env.values);
    }
    template<size_t I, typename Tags, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> const & env)
    {
        return std::get<I>(env.values);
    }
    template<size_t I, typename Tags, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> && env)
    {
        return std::get<I>(std::move(env.values));
    }
    template<size_t I, typename Tags, typename Tuple>
    requires(I < std::tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> const && env)
    {
        return std::get<I>(std::move(env.values));
    }

    template<environment Env, typename Tag>
    constexpr bool contains(Env const &, Tag)
    {
        return in_type_list<Tag, typename Env::tags_type>;
    }

    template<environment Env, typename Tag>
    constexpr bool contains(Tag)
    {
        return in_type_list<Tag, typename Env::tags_type>;
    }

    template<typename T, typename Env>
    concept tag_of = std::default_initializable<T> && contains<Env>(T{});

    template<
        environment Env,
        template<class...>
        class TypeList,
        typename... Tags2>
    requires type_list<TypeList<Tags2...>>
    constexpr bool contains_all_of(Env const &, TypeList<Tags2...>)
    {
        return (in_type_list<Tags2, typename Env::tags_type> && ...);
    }

    template<
        environment Env,
        template<class...>
        class TypeList,
        typename... Tags2>
    constexpr bool contains_all_of(TypeList<Tags2...>)
    {
        return (in_type_list<Tags2, typename Env::tags_type> && ...);
    }

    // clang-format off
    template<typename Tag, environment Env>
    requires tag_of<Tag, Env>
    constexpr decltype(auto) get(Env & e)
    {
        return e[Tag{}];
    }
    template<typename Tag, environment Env>
    requires tag_of<Tag, Env>
    constexpr decltype(auto) get(Env const & e)
    {
        return e[Tag{}];
    }
    template<typename Tag, environment Env>
    requires tag_of<Tag, Env>
    constexpr decltype(auto) get(Env && e)
    {
        return std::move(e)[Tag{}];
    }
    template<typename Tag, environment Env>
    requires tag_of<Tag, Env>
    constexpr decltype(auto) get(Env const && e)
    {
        return std::move(e)[Tag{}];
    }
    // clang-format on

    // clang-format off
    template<typename Tag, environment Env, typename T>
    requires (!tag_of<Tag, Env>)
    constexpr auto insert(Env const & e, Tag, T && x)
    {
        using tags_t = Env::tags_type;
        return env(
            detail::tl_append<std::remove_cvref_t<Tag>>(tags_t{}),
            tuple_cat(e.values, std::tuple((T &&) x)));
    }
    template<typename Tag, environment Env, typename T>
    requires (!tag_of<Tag, Env>)
    constexpr auto insert(Env && e, Tag, T && x)
    {
        using tags_t = Env::tags_type;
        return env(
            detail::tl_append<Tag>(tags_t{}),
            tuple_cat(std::move(e.values), std::tuple((T &&) x)));
    }
    // clang-format on

    // TODO: subset -> slice, filter -> with_only

    // clang-format off
    template<
        environment Env,
        template<typename...>
        typename TypeList,
        tag_of<Env>... Tags>
    requires type_list<TypeList<Tags...>>
    constexpr decltype(auto) subset(Env const& e, TypeList<Tags...> tags)
    // clang-format on
    {
        return env(tags, std::tuple(e[Tags{}]...));
    }

    // clang-format off
    template<
        environment Env,
        template<typename...>
        typename TypeList,
        tag_of<Env>... Tags>
    requires type_list<TypeList<Tags...>>
    constexpr decltype(auto) subset(Env&& e, TypeList<Tags...> tags)
    // clang-format on
    {
        return env(tags, std::tuple(std::move(e)[Tags{}]...));
    }

    template<environment Env, tag_of<Env>... Tags>
    constexpr decltype(auto) subset(Env const & e, Tags...)
    {
        using tags_t = Env::tags_type;
        return stdexec::subset(e, detail::tl_like(tags_t{}, Tags{}...));
    }

    template<environment Env, tag_of<Env>... Tags>
    constexpr decltype(auto) subset(Env && e, Tags...)
    {
        using tags_t = Env::tags_type;
        return stdexec::subset(
            std::move(e), detail::tl_like(tags_t{}, Tags{}...));
    }

    namespace detail {
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Tags1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Tags2>
        constexpr auto make_env_tuple(
            Env1 const & env1,
            TypeList1<Tags1...> old_tags,
            Env2 const & env2,
            TypeList2<Tags2...>)
        {
            return std::tuple(
                stdexec::get<Tags1>(env1)..., stdexec::get<Tags2>(env2)...);
        }
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Tags1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Tags2>
        constexpr auto make_env_tuple(
            Env1 && env1,
            TypeList1<Tags1...> old_tags,
            Env2 const & env2,
            TypeList2<Tags2...>)
        {
            return std::tuple(
                stdexec::get<Tags1>(std::move(env1))...,
                stdexec::get<Tags2>(env2)...);
        }
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Tags1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Tags2>
        constexpr auto make_env_tuple(
            Env1 const & env1,
            TypeList1<Tags1...> old_tags,
            Env2 && env2,
            TypeList2<Tags2...>)
        {
            return std::tuple(
                stdexec::get<Tags1>(env1)...,
                stdexec::get<Tags2>(std::move(env2))...);
        }
        template<
            typename Env1,
            template<class...>
            class TypeList1,
            typename... Tags1,
            typename Env2,
            template<class...>
            class TypeList2,
            typename... Tags2>
        constexpr auto make_env_tuple(
            Env1 && env1,
            TypeList1<Tags1...> old_tags,
            Env2 && env2,
            TypeList2<Tags2...>)
        {
            return std::tuple(
                stdexec::get<Tags1>(std::move(env1))...,
                stdexec::get<Tags2>(std::move(env2))...);
        }
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 const & e1, Env2 const & e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto old_tags = detail::tl_set_diff(tags1{}, tags2{});
        return env(
            detail::tl_cat(old_tags, tags2{}),
            detail::make_env_tuple(e1, old_tags, e2, tags2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 && e1, Env2 const & e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto old_tags = detail::tl_set_diff(tags1{}, tags2{});
        return env(
            detail::tl_cat(old_tags, tags2{}),
            detail::make_env_tuple(std::move(e1), old_tags, e2, tags2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 const & e1, Env2 && e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto old_tags = detail::tl_set_diff(tags1{}, tags2{});
        return env(
            detail::tl_cat(old_tags, tags2{}),
            detail::make_env_tuple(e1, old_tags, std::move(e2), tags2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert(Env1 && e1, Env2 && e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto old_tags = detail::tl_set_diff(tags1{}, tags2{});
        return env(
            detail::tl_cat(old_tags, tags2{}),
            detail::make_env_tuple(
                std::move(e1), old_tags, std::move(e2), tags2{}));
    }

    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 const & e1, Env2 const & e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto new_tags = detail::tl_set_diff(tags2{}, tags1{});
        return env(
            detail::tl_cat(tags1{}, new_tags),
            detail::make_env_tuple(e1, tags1{}, e2, new_tags));
    }
    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 const & e1, Env2 && e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto new_tags = detail::tl_set_diff(tags2{}, tags1{});
        return env(
            detail::tl_cat(tags1{}, new_tags),
            detail::make_env_tuple(e1, tags1{}, std::move(e2), new_tags));
    }
    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 && e1, Env2 const & e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto new_tags = detail::tl_set_diff(tags2{}, tags1{});
        return env(
            detail::tl_cat(tags1{}, new_tags),
            detail::make_env_tuple(std::move(e1), tags1{}, e2, new_tags));
    }
    template<environment Env1, environment Env2>
    constexpr auto insert_unique(Env1 && e1, Env2 && e2)
    {
        using tags1 = Env1::tags_type;
        using tags2 = Env2::tags_type;
        constexpr auto new_tags = detail::tl_set_diff(tags2{}, tags1{});
        return env(
            detail::tl_cat(tags1{}, new_tags),
            detail::make_env_tuple(
                std::move(e1), tags1{}, std::move(e2), new_tags));
    }

    template<typename Tag, environment Env>
    requires tag_of<Tag, Env>
    constexpr auto erase(Env const & e, Tag)
    {
        using tags_t = Env::tags_type;
        constexpr auto folded = detail::to_type_fold(tags_t{});
        constexpr int i = folded.index(detail::wrap<Tag>());
        return env(
            detail::tl_without_i<i>(folded, tags_t{}),
            detail::tuple_without_i<i>(e.values));
    }
    template<typename Tag, environment Env>
    requires tag_of<Tag, Env>
    constexpr auto erase(Env && e, Tag)
    {
        using tags_t = Env::tags_type;
        constexpr auto folded = detail::to_type_fold(tags_t{});
        constexpr int i = folded.index(detail::wrap<Tag>());
        return env(
            detail::tl_without_i<i>(folded, tags_t{}),
            detail::tuple_without_i<i>(std::move(e.values)));
    }

    template<
        environment Env,
        template<class...>
        class TypeList,
        tag_of<Env>... Tags>
    constexpr auto erase(Env const & e, TypeList<Tags...>)
    {
        using tags_t = Env::tags_type;
        constexpr auto remaining_tags =
            detail::tl_set_diff(tags_t{}, types<Tags...>{});
        return stdexec::subset(e, remaining_tags);
    }

    template<
        environment Env,
        template<class...>
        class TypeList,
        tag_of<Env>... Tags>
    constexpr auto erase(Env && e, TypeList<Tags...>)
    {
        using tags_t = Env::tags_type;
        constexpr auto remaining_tags =
            detail::tl_set_diff(tags_t{}, types<Tags...>{});
        return stdexec::subset(std::move(e), remaining_tags);
    }

    template<environment Env, tag_of<Env>... Tags>
    constexpr auto erase(Env const & e, Tags...)
    {
        return stdexec::erase(e, types<Tags...>{});
    }

    template<environment Env, tag_of<Env>... Tags>
    constexpr auto erase(Env && e, Tags...)
    {
        return stdexec::erase(std::move(e), types<Tags...>{});
    }



    // TODO: Add a value_type_env()?  It would convert a ref_env R to R.base_,
    // convert reference_wrapper<T> members of a ref to T,
    // std::string_view->string, etc.



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
        using tags_type = typename E::tags_type;

        ref_env() = default;
        ref_env(E & base) : base_(std::addressof(base)) {}
        ref_env(std::reference_wrapper<E> ref) : ref_env(ref.get()) {}

        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t)
        {
            return (*base_)[t];
        }

        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const
        {
            return std::as_const(*base_)[t];
        }

    private:
        E * base_;
    };

    template<environment E>
    ref_env(E &) -> ref_env<E>;
    template<environment E>
    ref_env(E const &) -> ref_env<E const>;
    template<environment E>
    ref_env(std::reference_wrapper<E>) -> ref_env<E>;

    namespace detail {
        template<typename F, typename Tags>
        constexpr bool invocable_with_all = false;
        template<
            typename F,
            template<typename...>
            typename TypeList,
            typename... Ts>
        constexpr bool invocable_with_all<F, TypeList<Ts...>> =
            (std::invocable<F, Ts> && ...);
    }

    template<typename F, type_list Tags>
    requires detail::invocable_with_all<F, Tags>
    struct computed_env
    {
        using tags_type = Tags;

        // clang-format off
        computed_env() requires std::default_initializable<F> = default;
        // clang-format on
        explicit computed_env(F f, Tags tags) : f_(f) {}

        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t)
        {
            return f_(t);
        }

        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const
        {
            return f_(t);
        }

    private:
        F f_;
    };

    template<environment E1, environment E2>
    struct layer_env
    {
        using tags_type = decltype(detail::tl_cat(
            typename E1::tags_type{},
            detail::tl_set_diff(
                typename E2::tags_type{}, typename E1::tags_type{})));

        // clang-format off
        layer_env()
        requires std::default_initializable<E1> &&
                 std::default_initializable<E2>
        = default;
        // clang-format on
        layer_env(E1 base_1, E2 base_2) :
            base_1_(std::move(base_1)), base_2_(std::move(base_2))
        {}

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag t)
        {
            if constexpr (in_type_list<Tag, typename E1::tags_type>)
                return ((Self &&) self).base_1_[t];
            else
                return ((Self &&) self).base_2_[t];
        }
#else
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) &
        {
            if constexpr (in_type_list<Tag, typename E1::tags_type>)
                return base_1_[t];
            else
                return base_2_[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const &
        {
            if constexpr (in_type_list<Tag, typename E1::tags_type>)
                return base_1_[t];
            else
                return base_2_[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) &&
        {
            if constexpr (in_type_list<Tag, typename E1::tags_type>)
                return std::move(base_1_)[t];
            else
                return std::move(base_2_)[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const &&
        {
            if constexpr (in_type_list<Tag, typename E1::tags_type>)
                return std::move(base_1_)[t];
            else
                return std::move(base_2_)[t];
        }
#endif

    private:
        E1 base_1_;
        E2 base_2_;
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
    template<environment E, type_list Tags>
    requires(contains_all_of<E>(Tags{}))
    struct filter_env
    // clang-format on
    {
        using tags_type = Tags;

        // clang-format off
        filter_env() requires std::default_initializable<E> = default;
        // clang-format on
        filter_env(E base, Tags tags) : base_(std::move(base)) {}

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag t)
        {
            return ((Self &&) self).base_[t];
        }
#else
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) &
        {
            return base_[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const &
        {
            return base_[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) &&
        {
            return std::move(base_)[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const &&
        {
            return std::move(base_)[t];
        }
#endif

    private:
        E base_;
    };

    template<typename E, typename Tags>
    filter_env(E, Tags) -> filter_env<E, Tags>;
    template<typename E, typename Tags>
    filter_env(std::reference_wrapper<E>, Tags) -> filter_env<ref_env<E>, Tags>;

    namespace detail {
        // clang-format off
        template<typename T, typename Tags>
        concept can_filter_env = requires {
            filter_env(std::declval<T>(), Tags{});
        };
        // clang-format on

        struct filter_impl : env_adaptor_closure<filter_impl>
        {
            template<env_or_ref E, type_list Tags>
            requires can_filter_env<E, Tags>
            constexpr auto operator()(E && e, Tags tags) const
            {
                return filter_env((E &&) e, tags);
            }
        };
    }

    inline constexpr detail::adaptor<detail::filter_impl>
        filter(detail::filter_impl{});

    template<environment E, type_list Tags>
    struct without_env
    {
        using tags_type =
            decltype(detail::tl_set_diff(typename E::tags_type{}, Tags{}));

        // clang-format off
        without_env() requires std::default_initializable<E> = default;
        // clang-format on
        without_env(E base, Tags tags) : base_(std::move(base)) {}

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag t)
        {
            return ((Self &&) self).base_[t];
        }
#else
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) &
        {
            return base_[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const &
        {
            return base_[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) &&
        {
            return std::move(base_)[t];
        }
        template<in_type_list<tags_type> Tag>
        constexpr decltype(auto) operator[](Tag t) const &&
        {
            return std::move(base_)[t];
        }
#endif

    private:
        E base_;
    };

    template<typename E, typename Tags>
    without_env(E, Tags) -> without_env<E, Tags>;
    template<typename E, typename Tags>
    without_env(std::reference_wrapper<E>, Tags)
        -> without_env<ref_env<E>, Tags>;

    namespace detail {
        // clang-format off
        template<typename T, typename Tags>
        concept can_without_env = requires {
            without_env(std::declval<T>(), Tags{});
        };
        // clang-format on

        struct without_impl : env_adaptor_closure<without_impl>
        {
            template<env_or_ref E, type_list Tags>
            requires can_without_env<E, Tags>
            constexpr auto operator()(E && e, Tags tags) const
            {
                return without_env((E &&) e, tags);
            }
        };
    }

    inline constexpr detail::adaptor<detail::without_impl>
        without(detail::without_impl{});
}

#if DO_TESTING

struct int_tag
{};
struct double_tag
{};
struct string_tag
{};

struct int_2_tag
{};
struct double_2_tag
{};
struct string_2_tag
{};

struct nullary_func_tag
{};
struct unary_func_tag
{};
struct binary_func_tag
{};

static_assert(
    !stdexec::detail::to_type_fold(stdexec::types<int_tag, double_tag>{}).has_dupes());
static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, int_tag>{})
        .has_dupes());
static_assert(
    !stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
         .has_dupes());

static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
        .index(stdexec::detail::wrap<int_tag>()) == 0);
static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
        .index(stdexec::detail::wrap<string_tag>()) == 1);
static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
        .index(stdexec::detail::wrap<double_tag>()) == 2);
static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
        .index(stdexec::detail::wrap<int>()) == -1);

static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
        .contains(stdexec::detail::wrap<int_tag>()));
static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
        .contains(stdexec::detail::wrap<string_tag>()));
static_assert(
    stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
        .contains(stdexec::detail::wrap<double_tag>()));
static_assert(
    !stdexec::detail::to_type_fold(stdexec::types<int_tag, string_tag, double_tag>{})
         .contains(stdexec::detail::wrap<int>()));

static_assert(std::same_as<
              decltype(stdexec::detail::to_type_fold(
                           stdexec::types<int_tag, string_tag, double_tag>{})
                           .type(stdexec::detail::cw<0>))::type,
              int_tag>);
static_assert(std::same_as<
              decltype(stdexec::detail::to_type_fold(
                           stdexec::types<int_tag, string_tag, double_tag>{})
                           .type(stdexec::detail::cw<1>))::type,
              string_tag>);
static_assert(std::same_as<
              decltype(stdexec::detail::to_type_fold(
                           stdexec::types<int_tag, string_tag, double_tag>{})
                           .type(stdexec::detail::cw<2>))::type,
              double_tag>);

#if HAVE_BOOST_MP11
using boost::mp11::mp_list;
#endif

TEST(env_, concept_)
{
    static_assert(stdexec::environment<decltype(stdexec::empty_env)>);

    stdexec::env env1 = stdexec::make_env(int_tag{}, 42)(double_tag{}, 13.0)(
        string_tag{}, std::string("foo"));

    static_assert(stdexec::environment<decltype(env1)>);
    static_assert(stdexec::environment<decltype(env1) const>);

    auto env2 = stdexec::env(
        stdexec::types<int_tag, double_tag, string_tag>{},
        std::tuple(42, 13.0, std::string("foo")));

    static_assert(stdexec::environment<decltype(env2)>);
    static_assert(stdexec::environment<decltype(env2) const>);

#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        static_assert(stdexec::environment<decltype(env1)>);
        static_assert(stdexec::environment<decltype(env1) const>);
    }
#endif
}

TEST(env_, make_env_)
{
    {
        stdexec::env env = stdexec::make_env(int_tag{}, 42)(double_tag{}, 13.0);

        auto const expected =
            stdexec::env(stdexec::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

        EXPECT_TRUE(env == expected);
    }
    {
        stdexec::env env = stdexec::make_env(int_tag{}, 42) //
            (double_tag{}, 13.0)                            //
            (string_tag{}, std::string("foo"))              //
            (int_2_tag{}, std::unique_ptr<int>());

        auto const expected = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag, int_2_tag>{},
            std::tuple(42, 13.0, std::string("foo"), std::unique_ptr<int>()));

        EXPECT_TRUE(env == expected);
    }
#if HAVE_BOOST_MP11
    {
        stdexec::env env =
            stdexec::make_env_with<mp_list>(int_tag{}, 42)(double_tag{}, 13.0);

        auto const expected =
            stdexec::env(mp_list<int_tag, double_tag>{}, std::tuple(42, 13.0));

        EXPECT_TRUE(env == expected);
    }
    {
        stdexec::env env = stdexec::make_env_with<mp_list>(int_tag{}, 42) //
            (double_tag{}, 13.0)                                          //
            (string_tag{}, std::string("foo"))                            //
            (int_2_tag{}, std::unique_ptr<int>());

        auto const expected = stdexec::env(
            mp_list<int_tag, double_tag, string_tag, int_2_tag>{},
            std::tuple(42, 13.0, std::string("foo"), std::unique_ptr<int>()));

        EXPECT_TRUE(env == expected);
    }
#endif
}

TEST(env_, type_get)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const & c_env = env;

            EXPECT_EQ(stdexec::get<int_tag>(c_env), 42);
            EXPECT_EQ(stdexec::get<double_tag>(c_env), 13.0);
            EXPECT_EQ(stdexec::get<string_tag>(c_env), "foo");

            static_assert(std::same_as<
                          decltype(stdexec::get<int_tag>(c_env)),
                          int const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_tag>(c_env)),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<int_tag>(std::move(c_env))),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_tag>(std::move(c_env))),
                          std::string const &&>);
        }

        EXPECT_EQ(stdexec::get<int_tag>(env), 42);
        EXPECT_EQ(stdexec::get<double_tag>(env), 13.0);
        EXPECT_EQ(stdexec::get<string_tag>(env), "foo");

        stdexec::get<int_tag>(env) = 8;
        EXPECT_EQ(stdexec::get<int_tag>(env), 8);

        stdexec::get<double_tag>(env) = 19.9;
        EXPECT_EQ(stdexec::get<double_tag>(env), 19.9);

        std::string const moved_to = stdexec::get<string_tag>(std::move(env));
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(stdexec::get<string_tag>(env), "");

        static_assert(
            std::same_as<decltype(stdexec::get<int_tag>(env)), int &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_tag>(env)),
                      std::string &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<int_tag>(std::move(env))),
                      int &&>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_tag>(std::move(env))),
                      std::string &&>);
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const & c_env = env;

            EXPECT_EQ(stdexec::get<int_tag>(c_env), 42);
            EXPECT_EQ(stdexec::get<double_tag>(c_env), 13.0);
            EXPECT_EQ(stdexec::get<string_tag>(c_env), "foo");

            static_assert(std::same_as<
                          decltype(stdexec::get<int_tag>(c_env)),
                          int const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_tag>(c_env)),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(stdexec::get<int_tag>(std::move(c_env))),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(stdexec::get<string_tag>(std::move(c_env))),
                          std::string const &&>);
        }

        EXPECT_EQ(stdexec::get<int_tag>(env), 42);
        EXPECT_EQ(stdexec::get<double_tag>(env), 13.0);
        EXPECT_EQ(stdexec::get<string_tag>(env), "foo");

        stdexec::get<int_tag>(env) = 8;
        EXPECT_EQ(stdexec::get<int_tag>(env), 8);

        stdexec::get<double_tag>(env) = 19.9;
        EXPECT_EQ(stdexec::get<double_tag>(env), 19.9);

        std::string const moved_to = stdexec::get<string_tag>(std::move(env));
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(stdexec::get<string_tag>(env), "");

        static_assert(
            std::same_as<decltype(stdexec::get<int_tag>(env)), int &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_tag>(env)),
                      std::string &>);
        static_assert(std::same_as<
                      decltype(stdexec::get<int_tag>(std::move(env))),
                      int &&>);
        static_assert(std::same_as<
                      decltype(stdexec::get<string_tag>(std::move(env))),
                      std::string &&>);
    }
#endif
}

TEST(env_, invocable_get)
{
    auto nullary = []() { return 77; };
    auto unary = [](unary_func_tag) { return 88; };

    {
        auto wrong_unary = [](int_tag) { return 99; };

        auto binary_impl = [](int a, int b) { return a + b; };
        std::function binary = binary_impl;

        auto env = stdexec::env(
            stdexec::types<
                int_tag,
                nullary_func_tag,
                unary_func_tag,
                binary_func_tag,
                int_2_tag>{},
            std::tuple(42, nullary, unary, binary, wrong_unary));

        EXPECT_EQ(stdexec::get<int_tag>(env), 42);
        EXPECT_EQ(stdexec::get<nullary_func_tag>(env), 77);
        EXPECT_EQ(stdexec::get<unary_func_tag>(env), 88);
        EXPECT_TRUE(
            stdexec::get<binary_func_tag>(env).target_type() ==
            binary.target_type());
        static_assert(
            std::invocable<decltype(stdexec::get<int_2_tag>(env)), int_tag>);
    }
    {
        auto env = stdexec::env(
            stdexec::types<int_tag, nullary_func_tag, unary_func_tag>{},
            std::tuple(
                42,
                stdexec::dont_invoke{nullary},
                stdexec::dont_invoke{unary}));

        static_assert(
            std::invocable<decltype(stdexec::get<nullary_func_tag>(env))>);
        static_assert(std::invocable<
                      decltype(stdexec::get<unary_func_tag>(env)),
                      unary_func_tag>);
    }
}

TEST(env_, nttp_get)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
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
            mp_list<int_tag, double_tag, string_tag>{},
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

TEST(env_, index_contains_equals)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        EXPECT_TRUE(env == env);

        {
            auto const & c_env = env;

            EXPECT_TRUE(env == c_env);

            EXPECT_EQ(stdexec::get<int_tag>(env), 42);
            EXPECT_EQ(stdexec::get<double_tag>(env), 13.0);
            EXPECT_EQ(stdexec::get<string_tag>(env), "foo");

            static_assert(
                std::same_as<decltype(c_env[int_tag{}]), int const &>);
            static_assert(std::same_as<
                          decltype(c_env[string_tag{}]),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[int_tag{}]),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[string_tag{}]),
                          std::string const &&>);
        }

        EXPECT_TRUE(stdexec::contains(env, int_tag{}));
        EXPECT_TRUE(stdexec::contains(env, double_tag{}));
        EXPECT_TRUE(stdexec::contains(env, string_tag{}));
        EXPECT_FALSE(stdexec::contains(env, int{}));

        EXPECT_TRUE(stdexec::contains_all_of(env, stdexec::types<int_tag>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<double_tag>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<string_tag>{}));

        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_tag, int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<double_tag, int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(
            stdexec::contains_all_of(env, stdexec::types<int_2_tag>{}));
        EXPECT_FALSE(stdexec::contains_all_of(
            env, stdexec::types<int_tag, int_2_tag>{}));

#if HAVE_BOOST_MP11
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<double_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<string_tag>{}));

        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<string_tag, int_tag>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<double_tag, int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, mp_list<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(stdexec::contains_all_of(env, mp_list<int_2_tag>{}));
        EXPECT_FALSE(
            stdexec::contains_all_of(env, mp_list<int_tag, int_2_tag>{}));
#endif

        EXPECT_EQ(env[int_tag{}], 42);
        EXPECT_EQ(env[double_tag{}], 13.0);
        EXPECT_EQ(env[string_tag{}], "foo");

        env[int_tag{}] = 8;
        EXPECT_EQ(env[int_tag{}], 8);

        env[double_tag{}] = 19.9;
        EXPECT_EQ(env[double_tag{}], 19.9);

        std::string const moved_to = std::move(env)[string_tag{}];
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(env[string_tag{}], "");

        static_assert(std::same_as<decltype(env[int_tag{}]), int &>);
        static_assert(std::same_as<decltype(env[string_tag{}]), std::string &>);
        static_assert(
            std::same_as<decltype(std::move(env)[int_tag{}]), int &&>);
        static_assert(std::same_as<
                      decltype(std::move(env)[string_tag{}]),
                      std::string &&>);
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        EXPECT_TRUE(env == env);

        {
            auto const & c_env = env;

            EXPECT_TRUE(env == c_env);

            EXPECT_EQ(stdexec::get<int_tag>(env), 42);
            EXPECT_EQ(stdexec::get<double_tag>(env), 13.0);
            EXPECT_EQ(stdexec::get<string_tag>(env), "foo");

            static_assert(
                std::same_as<decltype(c_env[int_tag{}]), int const &>);
            static_assert(std::same_as<
                          decltype(c_env[string_tag{}]),
                          std::string const &>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[int_tag{}]),
                          int const &&>);
            static_assert(std::same_as<
                          decltype(std::move(c_env)[string_tag{}]),
                          std::string const &&>);
        }

        EXPECT_TRUE(stdexec::contains(env, int_tag{}));
        EXPECT_TRUE(stdexec::contains(env, double_tag{}));
        EXPECT_TRUE(stdexec::contains(env, string_tag{}));
        EXPECT_FALSE(stdexec::contains(env, int{}));

        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<double_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(env, mp_list<string_tag>{}));

        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<string_tag, int_tag>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, mp_list<double_tag, int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, mp_list<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(stdexec::contains_all_of(env, mp_list<int_2_tag>{}));
        EXPECT_FALSE(
            stdexec::contains_all_of(env, mp_list<int_tag, int_2_tag>{}));

        EXPECT_TRUE(stdexec::contains_all_of(env, stdexec::types<int_tag>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<double_tag>{}));
        EXPECT_TRUE(
            stdexec::contains_all_of(env, stdexec::types<string_tag>{}));

        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_tag, int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<double_tag, int_tag>{}));
        EXPECT_TRUE(stdexec::contains_all_of(
            env, stdexec::types<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(
            stdexec::contains_all_of(env, stdexec::types<int_2_tag>{}));
        EXPECT_FALSE(stdexec::contains_all_of(
            env, stdexec::types<int_tag, int_2_tag>{}));

        EXPECT_EQ(env[int_tag{}], 42);
        EXPECT_EQ(env[double_tag{}], 13.0);
        EXPECT_EQ(env[string_tag{}], "foo");

        env[int_tag{}] = 8;
        EXPECT_EQ(env[int_tag{}], 8);

        env[double_tag{}] = 19.9;
        EXPECT_EQ(env[double_tag{}], 19.9);

        std::string const moved_to = std::move(env)[string_tag{}];
        EXPECT_EQ(moved_to, "foo");
        EXPECT_EQ(env[string_tag{}], "");

        static_assert(std::same_as<decltype(env[int_tag{}]), int &>);
        static_assert(std::same_as<decltype(env[string_tag{}]), std::string &>);
        static_assert(
            std::same_as<decltype(std::move(env)[int_tag{}]), int &&>);
        static_assert(std::same_as<
                      decltype(std::move(env)[string_tag{}]),
                      std::string &&>);
    }
#endif
}

TEST(env_, single_insert)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, double_tag, string_tag>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto inserted =
                stdexec::insert(env, string_tag{}, std::string("foo"));
            EXPECT_TRUE(inserted == expected);

            auto const expected_2 = stdexec::env(
                stdexec::types<int_tag, double_tag, string_tag, string_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), std::string("bar")));

            auto inserted_2 = stdexec::insert(
                std::move(inserted), string_2_tag{}, std::string("bar"));
            EXPECT_TRUE(inserted_2 == expected_2);

            auto const inserted_expected_after_move = stdexec::env(
                stdexec::types<int_tag, double_tag, string_tag>{},
                std::tuple(42, 13.0, std::string()));
            EXPECT_TRUE(inserted == inserted_expected_after_move);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto env =
            stdexec::env(mp_list<int_tag, double_tag>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                mp_list<int_tag, double_tag, string_tag>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto inserted =
                stdexec::insert(env, string_tag{}, std::string("foo"));
            EXPECT_TRUE(inserted == expected);

            auto const expected_2 = stdexec::env(
                mp_list<int_tag, double_tag, string_tag, string_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), std::string("bar")));

            auto inserted_2 = stdexec::insert(
                std::move(inserted), string_2_tag{}, std::string("bar"));
            EXPECT_TRUE(inserted_2 == expected_2);

            auto const inserted_expected_after_move = stdexec::env(
                mp_list<int_tag, double_tag, string_tag>{},
                std::tuple(42, 13.0, std::string()));
            EXPECT_TRUE(inserted == inserted_expected_after_move);
        }
    }
#endif
}

TEST(env_, subset)
{
    {
        auto env = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto subset = stdexec::subset(
                env, stdexec::types<int_tag, double_tag, string_tag>{});
            EXPECT_TRUE(subset == env);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto subset = stdexec::subset(
                env, stdexec::types<int_tag, string_tag, double_tag>{});
            EXPECT_TRUE(subset == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<string_tag, double_tag>{},
                std::tuple(std::string("foo"), 13.0));

            auto subset = stdexec::subset(env, string_tag{}, double_tag{});
            EXPECT_TRUE(subset == expected);
        }
#if HAVE_BOOST_MP11
        {
            auto const expected = stdexec::env(
                mp_list<int_tag, double_tag, string_tag>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto subset = stdexec::subset(
                env, mp_list<int_tag, double_tag, string_tag>{});
            EXPECT_TRUE(subset == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto subset = stdexec::subset(
                env, mp_list<int_tag, string_tag, double_tag>{});
            EXPECT_TRUE(subset == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<string_tag, double_tag>{},
                std::tuple(std::string("foo"), 13.0));

            auto subset = stdexec::subset(env, string_tag{}, double_tag{});
            EXPECT_TRUE(subset == expected);
        }
#endif
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, double_tag, string_tag>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto subset = stdexec::subset(
                env, stdexec::types<int_tag, double_tag, string_tag>{});
            EXPECT_TRUE(subset == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto subset = stdexec::subset(
                env, stdexec::types<int_tag, string_tag, double_tag>{});
            EXPECT_TRUE(subset == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<string_tag, double_tag>{},
                std::tuple(std::string("foo"), 13.0));

            auto subset = stdexec::subset(env, string_tag{}, double_tag{});
            EXPECT_TRUE(subset == expected);
        }
#if HAVE_BOOST_MP11
        {
            auto const expected = stdexec::env(
                mp_list<int_tag, double_tag, string_tag>{},
                std::tuple(42, 13.0, std::string("foo")));

            auto subset = stdexec::subset(
                env, mp_list<int_tag, double_tag, string_tag>{});
            EXPECT_TRUE(subset == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("foo"), 13.0));

            auto subset = stdexec::subset(
                env, mp_list<int_tag, string_tag, double_tag>{});
            EXPECT_TRUE(subset == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<string_tag, double_tag>{},
                std::tuple(std::string("foo"), 13.0));

            auto subset = stdexec::subset(env, string_tag{}, double_tag{});
            EXPECT_TRUE(subset == expected);
        }
#endif
    }
#endif
}

// TODO: property concept from Lewis's godbolt.

TEST(env_, insert_env)
{
    {
        auto const env1 = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == env2);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto const env1 = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, double_tag, string_tag>{},
                std::tuple(120, 19.9, std::string("bar")));

            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                stdexec::types<
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

        {
            auto const expected = stdexec::env(
                mp_list<int_tag, double_tag, string_tag>{},
                std::tuple(120, 19.9, std::string("bar")));

            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

        {
            auto result = stdexec::insert(env1, env2);
            EXPECT_TRUE(result == env2);
        }
        {
            auto const expected = stdexec::env(
                mp_list<int_tag, string_tag, double_tag>{},
                std::tuple(42, std::string("baz"), 58.0));

            auto result = stdexec::insert(env1, env1_less);
            EXPECT_TRUE(result == expected);
        }
        {
            auto const expected = stdexec::env(
                mp_list<
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
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
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

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
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert_unique(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto const env1 = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

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
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert_unique(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            stdexec::types<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            stdexec::types<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

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
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
                std::tuple(42, 13.0, std::string("foo"), 42, 13.0));

            auto result = stdexec::insert_unique(env1, env1_more);
            EXPECT_TRUE(result == expected);
        }
    }
    {
        auto const env1 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));
        auto const env2 = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(120, 19.9, std::string("bar")));

        auto const env1_less = stdexec::env(
            mp_list<string_tag, double_tag>{},
            std::tuple(std::string("baz"), 58.0));
        auto const env1_more = stdexec::env(
            mp_list<int_2_tag, double_2_tag>{}, std::tuple(42, 13.0));

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
                    int_tag,
                    double_tag,
                    string_tag,
                    int_2_tag,
                    double_2_tag>{},
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
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

            auto erased = stdexec::erase(env, string_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                stdexec::types<int_tag, string_tag>{},
                std::tuple(42, std::string("foo")));

            auto erased = stdexec::erase(env, double_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                stdexec::types<double_tag, string_tag>{},
                std::tuple(13.0, std::string("foo")));

            auto erased = stdexec::erase(env, int_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto erased_1 = stdexec::erase(env, int_tag{});
            auto erased_2 = stdexec::erase(erased_1, double_tag{});
            auto final_ = stdexec::erase(erased_2, string_tag{});
            EXPECT_TRUE(final_ == stdexec::empty_env);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto env = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto const expected = stdexec::env(
                mp_list<int_tag, double_tag>{}, std::tuple(42, 13.0));

            auto erased = stdexec::erase(env, string_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                mp_list<int_tag, string_tag>{},
                std::tuple(42, std::string("foo")));

            auto erased = stdexec::erase(env, double_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto const expected = stdexec::env(
                mp_list<double_tag, string_tag>{},
                std::tuple(13.0, std::string("foo")));

            auto erased = stdexec::erase(env, int_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto erased_1 = stdexec::erase(env, int_tag{});
            auto erased_2 = stdexec::erase(erased_1, double_tag{});
            auto final_ = stdexec::erase(erased_2, string_tag{});
            EXPECT_TRUE(final_ == (stdexec::env<mp_list<>, std::tuple<>>()));
        }
    }
#endif
}

TEST(env_, multi_erase)
{
    {
        auto const initial_env = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                stdexec::types<string_tag>{}, std::tuple(std::string("foo")));

            auto erased = stdexec::erase(env, int_tag{}, double_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                stdexec::types<string_tag>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(std::move(env), double_tag{}, int_tag{});
            EXPECT_TRUE(erased == expected);

            EXPECT_EQ(env[string_tag{}], std::string());
        }

        {
            auto env = initial_env;

            auto const expected =
                stdexec::env(stdexec::types<double_tag>{}, std::tuple(13.0));

            auto erased = stdexec::erase(env, int_tag{}, string_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                stdexec::types<string_tag>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(env, string_tag{}, double_tag{}, int_tag{});
            EXPECT_TRUE(erased == stdexec::empty_env);
        }
    }
#if HAVE_BOOST_MP11
    {
        auto const initial_env = stdexec::env(
            mp_list<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                mp_list<string_tag>{}, std::tuple(std::string("foo")));

            auto erased = stdexec::erase(env, int_tag{}, double_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                mp_list<string_tag>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(std::move(env), double_tag{}, int_tag{});
            EXPECT_TRUE(erased == expected);

            EXPECT_EQ(env[string_tag{}], std::string());
        }

        {
            auto env = initial_env;

            auto const expected =
                stdexec::env(mp_list<double_tag>{}, std::tuple(13.0));

            auto erased = stdexec::erase(env, int_tag{}, string_tag{});
            EXPECT_TRUE(erased == expected);
        }

        {
            auto env = initial_env;

            auto const expected = stdexec::env(
                mp_list<string_tag>{}, std::tuple(std::string("foo")));

            auto erased =
                stdexec::erase(env, string_tag{}, double_tag{}, int_tag{});
            EXPECT_TRUE(erased == (stdexec::env<mp_list<>, std::tuple<>>()));
        }
    }
#endif
}

TEST(env_, ref_env_)
{
     {
         auto env = stdexec::env(
             stdexec::types<int_tag, double_tag, string_tag>{},
             std::tuple(42, 13.0, std::string("foo")));

         stdexec::ref_env ref1 = env;
         stdexec::ref_env ref2 = std::ref(env);

         EXPECT_EQ(ref1[int_tag{}], env[int_tag{}]);
         EXPECT_EQ(ref1[double_tag{}], env[double_tag{}]);
         EXPECT_EQ(ref1[string_tag{}], env[string_tag{}]);

         EXPECT_EQ(ref1[int_tag{}], ref2[int_tag{}]);
         EXPECT_EQ(ref1[double_tag{}], ref2[double_tag{}]);
         EXPECT_EQ(ref1[string_tag{}], ref2[string_tag{}]);

         {
             auto const & c_env = env;

             stdexec::ref_env ref1 = c_env;
             stdexec::ref_env ref2 = std::ref(c_env);

             EXPECT_EQ(ref1[int_tag{}], env[int_tag{}]);
             EXPECT_EQ(ref1[double_tag{}], env[double_tag{}]);
             EXPECT_EQ(ref1[string_tag{}], env[string_tag{}]);

             EXPECT_EQ(ref1[int_tag{}], ref2[int_tag{}]);
             EXPECT_EQ(ref1[double_tag{}], ref2[double_tag{}]);
             EXPECT_EQ(ref1[string_tag{}], ref2[string_tag{}]);
         }
     }
}

TEST(env_, computed_env_)
{
    auto universal = [](auto) { return 42; };

    auto env = stdexec::computed_env(
        universal, stdexec::types<int_tag, double_tag, string_tag>{});

    static_assert(stdexec::contains_all_of<decltype(env)>(
        typename decltype(env)::tags_type{}));

    EXPECT_EQ(env[int_tag{}], universal(int_tag{}));
    EXPECT_EQ(env[double_tag{}], universal(double_tag{}));
    EXPECT_EQ(env[string_tag{}], universal(string_tag{}));
}

TEST(env_, layer_env_)
{
     {
         auto int_env = stdexec::env(stdexec::types<int_tag>{}, std::tuple(42));

         auto double_env =
             stdexec::env(stdexec::types<double_tag>{}, std::tuple(13.0));

         {
             auto env = stdexec::layer_env(int_env, double_env);

             static_assert(std::same_as<decltype(env),
             stdexec::layer_env<
                 stdexec::env<stdexec::types<int_tag>, std::tuple<int>>,
                           stdexec::env<stdexec::types<double_tag>, std::tuple<double>>>>);

             EXPECT_EQ(env[int_tag{}], int_env[int_tag{}]);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
         }
         {
             auto env = int_env | stdexec::layer(double_env);
             static_assert(
                 std::same_as<
                     decltype(env),
                     stdexec::layer_env<
                         stdexec::env<stdexec::types<int_tag>, std::tuple<int>>,
                         stdexec::env<
                             stdexec::types<double_tag>,
                             std::tuple<double>>>>);

             EXPECT_EQ(env[int_tag{}], int_env[int_tag{}]);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
         }
         {
             auto env = int_env | stdexec::layer(std::ref(double_env));
             static_assert(
                 std::same_as<
                     decltype(env),
                     stdexec::layer_env<
                         stdexec::env<stdexec::types<int_tag>, std::tuple<int>>,
                         stdexec::ref_env<stdexec::env<
                             stdexec::types<double_tag>,
                             std::tuple<double>>>>>);

             EXPECT_EQ(env[int_tag{}], int_env[int_tag{}]);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
         }
         {
             auto env = std::ref(int_env) | stdexec::layer(double_env);
             static_assert(std::same_as<
                           decltype(env),
                           stdexec::layer_env<
                               stdexec::ref_env<stdexec::env<
                                   stdexec::types<int_tag>,
                                   std::tuple<int>>>,
                               stdexec::env<
                                   stdexec::types<double_tag>,
                                   std::tuple<double>>>>);

             EXPECT_EQ(env[int_tag{}], int_env[int_tag{}]);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
         }
         {
             auto env =
                 std::ref(int_env) | stdexec::layer(std::ref(double_env));
             static_assert(std::same_as<
                           decltype(env),
                           stdexec::layer_env<
                               stdexec::ref_env<stdexec::env<
                                   stdexec::types<int_tag>,
                                   std::tuple<int>>>,
                               stdexec::ref_env<stdexec::env<
                                   stdexec::types<double_tag>,
                                   std::tuple<double>>>>>);

             EXPECT_EQ(env[int_tag{}], int_env[int_tag{}]);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
         }

         auto string_env = stdexec::env(
             stdexec::types<string_tag>{}, std::tuple(std::string("foo")));

         {
             auto env = int_env | stdexec::layer(double_env) |
                        stdexec::layer(string_env);

             EXPECT_EQ(env[int_tag{}], int_env[int_tag{}]);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
             EXPECT_EQ(env[string_tag{}], string_env[string_tag{}]);
         }

         {
             auto int_double_env = stdexec::env(
                 stdexec::types<int_tag, double_tag>{}, std::tuple(55, 13.0));

             auto const & c_string_env = string_env;

             auto env = int_double_env | stdexec::layer(std::ref(double_env)) |
                        stdexec::layer(std::ref(c_string_env));

             EXPECT_EQ(env[int_tag{}], 55);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
             EXPECT_EQ(env[string_tag{}], string_env[string_tag{}]);
         }

         {
             auto env = int_env | stdexec::layer(double_env) |
                 stdexec::layer(std::move(string_env));

             EXPECT_EQ(env[int_tag{}], int_env[int_tag{}]);
             EXPECT_EQ(env[double_tag{}], double_env[double_tag{}]);
             EXPECT_EQ(env[string_tag{}], "foo");
             EXPECT_EQ(string_env[string_tag{}], "");
         }
     }
}

TEST(env_, filter_env_)
{
    {
        auto const initial_env = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env = stdexec::filter(
                initial_env, stdexec::types<string_tag, int_tag>{});

            static_assert(!stdexec::contains(env, double_tag{}));
            EXPECT_EQ(env[int_tag{}], initial_env[int_tag{}]);
            EXPECT_EQ(env[string_tag{}], initial_env[string_tag{}]);
        }
    }
}

TEST(env_, without_env_)
{
    {
        auto const initial_env = stdexec::env(
            stdexec::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        {
            auto env =
                initial_env | stdexec::without(stdexec::types<double_tag>{});

            static_assert(!stdexec::contains(env, double_tag{}));
            EXPECT_EQ(env[int_tag{}], initial_env[int_tag{}]);
            EXPECT_EQ(env[string_tag{}], initial_env[string_tag{}]);
        }
    }
}

#endif
