#include <iostream>
#include <ranges>
#include <string>
#include <tuple>
#include <utility>


namespace std {
    namespace detail {
        template<typename T>
        constexpr bool has_type_params = false;
        template<template<class...> class Template, typename... Ts>
        constexpr bool has_type_params<Template<Ts...>> = true;

        template<typename T>
        concept env_value = same_as<decay_t<T>, T>;
        /* regular, a std::reference_wrapper, or value_type_for<E>
            is well-formed, and refers to some type E2 such that E2(E) is
           regular an well-formed.  E2 must not refer to any other object. */

        template<typename T>
        constexpr bool tuple_args_env_vals = false;
        template<typename... Ts>
        constexpr bool
            tuple_args_env_vals<tuple<Ts...>> = (env_value<Ts> && ...);

        template<template<typename...> typename TypeList, typename... Ts>
        constexpr bool has_dupes(TypeList<Ts...>)
        {
            // TODO: add use new metaprogramming to make a type Checker that
            // contains data members that are [[no_unique_address]]
            // wrapper<Ts>..., and then check that sizeof(Checker) == 1u.
            return false;
        }

        template<
            typename Tag,
            int I,
            template<typename...>
            typename TypeList,
            typename T,
            typename... Ts>
        constexpr int index_from_tag(TypeList<T, Ts...>)
        {
#if 0 // TODO
            meta::info types[] = {^T, ^Ts...};
            auto it = ranges::find(types, ^Tag);
            static_assert(it != ranges::end(types));
            return it - types.begin();
#endif
            if constexpr (same_as<Tag, T>)
                return I;
            else if constexpr (!sizeof...(Ts))
                return -1;
            else
                return index_from_tag<Tag, I + 1>(TypeList<Ts...>{});
        }
    }

    template<typename T>
    concept type_list = std::is_empty_v<T> && detail::has_type_params<T>;

    template<typename T>
    struct value_type_for
    {
        using type = T;
    };
    template<typename T>
    struct value_type_for<reference_wrapper<T>>
    {
        using type = T;
    };
    template<typename T>
    using value_type_for_t = value_type_for<T>::type;

    template<typename T>
    concept env_tuple = detail::tuple_args_env_vals<T>;

    // clang-format off
    template<typename T>
    concept queryable_environment = requires(T x) {
        { x.tags } -> type_list;
        { x.values } -> env_tuple;
    };

    namespace detail {
        template<typename T>
        concept free_queryable = requires(T & x)
        {
            { make_queryable_environment(x) } -> queryable_environment;
        };
        template<typename T>
        concept member_queryable = requires(T & x)
        {
            { x.make_queryable_environment() } -> queryable_environment;
        };
        // clang-format on

        template<typename Tag, typename Tags>
        constexpr bool has_tag = 0 <= detail::index_from_tag<Tag, 0>(Tags{});

        template<typename T, template<class...> class TypeList, typename... Ts>
        constexpr auto tl_append(TypeList<Ts...>)
        {
            return TypeList<Ts..., T>{};
        }

        template<
            typename X,
            template<class...>
            class TypeList,
            typename T,
            typename... Ts,
            typename... Us>
        constexpr auto tl_erase_impl(TypeList<Us...>)
        {
            if constexpr (same_as<X, T>)
                return TypeList<Us..., Ts...>{};
            else if constexpr (sizeof...(Ts) == 0)
                return TypeList<Us..., T>{};
            else
                return tl_erase_impl<X, TypeList, Ts...>(TypeList<Us..., T>{});
        }
        template<typename T, template<class...> class TypeList, typename... Ts>
        constexpr auto tl_erase(TypeList<Ts...>)
        {
            return tl_erase_impl<T, TypeList, Ts...>(TypeList<>{});
        }

        template<int JsOffset, int... Is, int... Js>
        auto
        glom_indices(integer_sequence<int, Is...>, integer_sequence<int, Js...>)
        {
            return integer_sequence<int, Is..., Js + JsOffset...>{};
        }
        template<int I, int TupleSize>
        constexpr auto tuple_indices_without_i()
        {
            return glom_indices<I + 1>(
                make_integer_sequence<int, I>{},
                make_integer_sequence<int, TupleSize - (I + 1)>{});
        }
        template<typename Tuple, int... Is>
        auto tuple_without_i_impl(Tuple && t, integer_sequence<int, Is...>)
        {
            return tuple(std::get<Is>((Tuple &&) t)...);
        }
        template<int I, typename Tuple>
        auto tuple_without_i(Tuple && t)
        {
            return tuple_without_i_impl(
                (Tuple &&) t,
                tuple_indices_without_i<
                    I,
                    tuple_size_v<remove_cvref_t<Tuple>>>());
        }

        template<
            template<class...>
            class TypeList,
            typename... Tags2,
            typename... UniqueTags>
        auto set_diff_tags(
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
        auto set_diff_tags(
            TypeList<Tag, Tags1...> tl1,
            TypeList<Tags2...> tl2,
            TypeList<UniqueTags...> result)
        {
            constexpr int i = index_from_tag<Tag, 0>(Tags2{});
            if constexpr (0 <= i) {
                return set_diff_tags(
                    TypeList<Tags1...>{}, tl2, TypeList<UniqueTags...>{});
            } else {
                return set_diff_tags(
                    TypeList<Tags1...>{}, tl2, TypeList<UniqueTags..., Tag>{});
            }
        }
        template<
            template<class...>
            class TypeList,
            typename... Tags1,
            typename... Tags2>
        auto glom_tags(TypeList<Tags1...>, TypeList<Tags2...>)
        {
            return TypeList<Tags1..., Tags2...>{};
        }
        template<
            template<class...>
            class TypeList,
            typename... Tags1,
            typename... Tags2>
        auto union_of_tags(TypeList<Tags1...> tl1, TypeList<Tags2...> tl2)
        {
            return glom_tags(tl2, set_diff_tags(tl1, tl2, TypeList<>{}));
        }
    }

    template<typename T>
    concept queryable =
        detail::free_queryable<T> || detail::member_queryable<T>;

    template<typename... Ts>
    struct types
    {};

    template<type_list Tags, env_tuple Tuple>
    struct default_queryable_environment
    {
        constexpr bool
        operator==(default_queryable_environment const &) const = default;

        Tags tags;
        Tuple values;
    };

    template<typename Tag, typename Tags, typename Tuple>
    constexpr bool has_tag(default_queryable_environment<Tags, Tuple> const &)
    {
        return detail::has_tag<Tag, Tags>;
    }

    template<typename Tag, typename Tags, typename Tuple, typename T>
    constexpr auto
    insert(default_queryable_environment<Tags, Tuple> const & env, T && x)
    {
        return default_queryable_environment(
            detail::tl_append<remove_cvref_t<Tag>>(Tags{}),
            tuple_cat(env.values, tuple((T &&) x)));
    }
    template<typename Tag, typename Tags, typename Tuple, typename T>
    constexpr auto
    insert(default_queryable_environment<Tags, Tuple> && env, T && x)
    {
        return default_queryable_environment(
            detail::tl_append<Tag>(Tags{}),
            tuple_cat(std::move(env.values), tuple((T &&) x)));
    }

    template<typename Tag, typename Tags, typename Tuple, typename T>
    requires detail::has_tag<Tag, Tags>
    constexpr void
    assign(default_queryable_environment<Tags, Tuple> & env, T && x)
    {
        std::get<Tag>(env) = (T &&) x;
    }

#if 0
    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto insert(
        default_queryable_environment<Tags1, Tuple1> const & env1,
        default_queryable_environment<Tags2, Tuple2> const & env2)
    {
        
    }

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto insert_unique(
        default_queryable_environment<Tags1, Tuple1> const & env1,
        default_queryable_environment<Tags2, Tuple2> const & env2)
    {
        
    }
#endif

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr auto erase(default_queryable_environment<Tags, Tuple> const & env)
    {
        constexpr int i = detail::index_from_tag<Tag, 0>(Tags{});
        return default_queryable_environment(
            detail::tl_erase<Tag>(Tags{}),
            detail::tuple_without_i<i>(env.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr auto erase(default_queryable_environment<Tags, Tuple> && env)
    {
        constexpr int i = detail::index_from_tag<Tag, 0>(Tags{});
        return default_queryable_environment(
            detail::tl_erase<Tag>(Tags{}),
            detail::tuple_without_i<i>(std::move(env.values)));
    }

    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto)
    get(default_queryable_environment<Tags, Tuple> & env)
    {
        constexpr int i = detail::index_from_tag<Tag, 0>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto)
    get(default_queryable_environment<Tags, Tuple> const & env)
    {
        constexpr int i = detail::index_from_tag<Tag, 0>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto)
    get(default_queryable_environment<Tags, Tuple> && env)
    {
        constexpr int i = detail::index_from_tag<Tag, 0>(Tags{});
        return std::get<i>(std::move(env.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto)
    get(default_queryable_environment<Tags, Tuple> const && env)
    {
        constexpr int i = detail::index_from_tag<Tag, 0>(Tags{});
        return std::get<i>(std::move(env.values));
    }
}

// Eric Niebler — Today at 5:02 PM
// for a general purpose utility you really only need a few things: a way to
// create an env from a query and a value, a way to join two envs, and a way
// to remove a query from an env. i have also found useful a
// reference_wrapper-like env and, for p2300, a forwarding env that filters
// out non-forwarding queries.

struct int_tag;
struct double_tag;
struct string_tag;

int main()
{
    auto env = std::default_queryable_environment(
        std::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

    std::cout << std::get<int_tag>(env) << "\n";
    std::cout << std::get<double_tag>(env) << "\n";

    {
        auto str_env = std::insert<string_tag>(env, std::string("text"));
        std::cout << std::get<string_tag>(str_env) << "\n";

        {
            auto no_dbl_env = std::erase<double_tag>(str_env);
            std::cout << std::get<int_tag>(no_dbl_env) << "\n";
            std::cout << std::get<string_tag>(no_dbl_env) << "\n";
        }
    }

    return 0;
}
