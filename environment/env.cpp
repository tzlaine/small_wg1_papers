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
            return (type_fold<T>{} + ... + wrap<Ts>());
        }

        template<
            typename Tag,
            template<typename...>
            typename TypeList,
            typename T,
            typename... Ts>
        consteval int index_from_tag(TypeList<T, Ts...> tl)
        {
            return to_type_fold(tl).index(wrap<Tag>());
        }
    }

    template<typename T>
    concept type_list =
        is_empty_v<T> && detail::has_type_params<T> && semiregular<T>;

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
        concept free_queryable = requires(T & x) {
            { make_queryable_environment(x) } -> queryable_environment;
        };
        template<typename T>
        concept member_queryable = requires(T & x) {
            { x.make_queryable_environment() } -> queryable_environment;
        };
        // clang-format on

        template<typename Tag, typename Tags>
        constexpr bool has_tag = 0 <= detail::index_from_tag<Tag>(Tags{});

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

        template<
            typename X,
            template<class...>
            class TypeList,
            typename T,
            typename... Ts,
            typename... Us>
        consteval auto tl_erase_impl(TypeList<Us...>)
        {
            if constexpr (same_as<X, T>)
                return TypeList<Us..., Ts...>{};
            else if constexpr (sizeof...(Ts) == 0)
                return TypeList<Us..., T>{};
            else
                return tl_erase_impl<X, TypeList, Ts...>(TypeList<Us..., T>{});
        }
        template<typename T, template<class...> class TypeList, typename... Ts>
        consteval auto tl_erase(TypeList<Ts...>)
        {
            return tl_erase_impl<T, TypeList, Ts...>(TypeList<>{});
        }

        template<
            typename... Us,
            template<typename...>
            typename TypeList,
            typename... Ts>
        consteval auto tl_like(TypeList<Ts...>)
        {
            return TypeList<Us...>{};
        }

        template<int JsOffset, int... Is, int... Js>
        consteval auto
        cat_indices(integer_sequence<int, Is...>, integer_sequence<int, Js...>)
        {
            return integer_sequence<int, Is..., Js + JsOffset...>{};
        }
        template<int I, int TupleSize>
        consteval auto tuple_indices_without_i()
        {
            return cat_indices<I + 1>(
                make_integer_sequence<int, I>{},
                make_integer_sequence<int, TupleSize - (I + 1)>{});
        }
        template<typename Tuple, int... Is>
        constexpr auto
        tuple_without_i_impl(Tuple && t, integer_sequence<int, Is...>)
        {
            return tuple(std::get<Is>((Tuple &&) t)...);
        }
        template<int I, typename Tuple>
        constexpr auto tuple_without_i(Tuple && t)
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
                return tl_set_diff_impl(
                    TypeList<Tags1...>{}, tl2, TypeList<UniqueTags...>{});
            } else {
                return tl_set_diff_impl(
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
            return tl_set_diff_impl(tl1, TypeList1<Tags2...>{}, TypeList1<>{});
        }
        template<
            template<class...>
            class TypeList,
            typename... Tags1,
            typename... Tags2>
        consteval auto
        union_of_tags(TypeList<Tags1...> tl1, TypeList<Tags2...> tl2)
        {
            return tl_cat(tl2, tl_set_diff(tl1, tl2));
        }
    }

    template<typename T>
    concept queryable =
        detail::free_queryable<T> || detail::member_queryable<T>;

    template<typename... Ts>
    struct types
    {};

    template<type_list Tags, env_tuple Tuple>
    struct env
    {
        constexpr bool operator==(env const &) const = default;

        template<typename Tag>
        constexpr bool contains(Tag) const
        {
            return detail::has_tag<Tag, Tags>;
        }

        template<typename Tag>
        constexpr decltype(auto) operator[](Tag) &
        {
            return std::get<Tag>(*this);
        }
        template<typename Tag>
        constexpr decltype(auto) operator[](Tag) const &
        {
            return std::get<Tag>(*this);
        }
        template<typename Tag>
        constexpr decltype(auto) operator[](Tag) &&
        {
            return std::get<Tag>(*this);
        }

        // TODO -> free fn.
        template<typename... Tags2>
        constexpr decltype(auto) subset(Tags2...)
        {
            return env(
                detail::tl_like<Tags2...>(tags), tuple(operator[](Tags2{})...));
        }

        template<template<typename...> typename TypeList, typename... Tags2>
        requires type_list<TypeList<Tags2...>>
        constexpr decltype(auto) subset(TypeList<Tags2...> tags2)
        {
            return env(tags2, tuple(operator[](Tags2{})...));
        }

        Tags tags;
        Tuple values;
    };

    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto) get(env<Tags, Tuple> & env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto) get(env<Tags, Tuple> const & env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto) get(env<Tags, Tuple> && env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(std::move(env.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    constexpr decltype(auto) get(env<Tags, Tuple> const && env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(std::move(env.values));
    }

    template<typename Tag, typename Tags, typename Tuple, typename T>
    constexpr auto insert(env<Tags, Tuple> const & env_, T && x)
    {
        return env(
            detail::tl_append<remove_cvref_t<Tag>>(Tags{}),
            tuple_cat(env_.values, tuple((T &&) x)));
    }
    template<typename Tag, typename Tags, typename Tuple, typename T>
    constexpr auto insert(env<Tags, Tuple> && env, T && x)
    {
        return env(
            detail::tl_append<Tag>(Tags{}),
            tuple_cat(std::move(env.values), tuple((T &&) x)));
    }

    namespace detail {
        template<
            typename Tags1,
            typename Tuple1,
            int... Is,
            typename Tags2,
            typename Tuple2,
            template<class...>
            class TypeList,
            typename... NewTags>
        constexpr auto make_env_tuple(
            env<Tags1, Tuple1> const & env1,
            integer_sequence<int, Is...>,
            env<Tags2, Tuple2> const & env2,
            TypeList<NewTags...> new_tags)
        {
            return tuple(
                std::get<Is>(env1.values)..., std::get<NewTags>(env2)...);
        }
    }

#if 0
    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert(env<Tags1, Tuple1> const & env1, env<Tags2, Tuple2> const & env2)
    {
        
    }
#endif

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto insert_unique(
        env<Tags1, Tuple1> const & env1, env<Tags2, Tuple2> const & env2)
    {
        constexpr auto new_tags =
            detail::tl_set_diff(decltype(env2.tags){}, decltype(env1.tags){});
        return env(
            detail::tl_cat(decltype(env1.tags){}, new_tags),
            detail::make_env_tuple(
                env1,
                make_integer_sequence<int, tuple_size_v<Tuple1>>{},
                env2,
                new_tags));
    }

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr auto erase(env<Tags, Tuple> const & env_)
    {
        constexpr int i = detail::index_from_tag<Tag>(Tags{});
        return env(
            detail::tl_erase<Tag>(Tags{}),
            detail::tuple_without_i<i>(env_.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr auto erase(env<Tags, Tuple> && env_)
    {
        constexpr int i = detail::index_from_tag<Tag>(Tags{});
        return env(
            detail::tl_erase<Tag>(Tags{}),
            detail::tuple_without_i<i>(std::move(env_.values)));
    }

    // TODO: Multi-erase.
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

static_assert(
    !std::detail::to_type_fold(std::types<int_tag, double_tag>{}).has_dupes());
static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, int_tag>{})
        .has_dupes());
static_assert(
    !std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
         .has_dupes());

static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
        .index(std::detail::wrap<int_tag>()) == 0);
static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
        .index(std::detail::wrap<string_tag>()) == 1);
static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
        .index(std::detail::wrap<double_tag>()) == 2);
static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
        .index(std::detail::wrap<int>()) == -1);

static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
        .contains(std::detail::wrap<int_tag>()));
static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
        .contains(std::detail::wrap<string_tag>()));
static_assert(
    std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
        .contains(std::detail::wrap<double_tag>()));
static_assert(
    !std::detail::to_type_fold(std::types<int_tag, string_tag, double_tag>{})
         .contains(std::detail::wrap<int>()));

static_assert(std::same_as<
              decltype(std::detail::to_type_fold(
                           std::types<int_tag, string_tag, double_tag>{})
                           .type(std::detail::cw<0>))::type,
              int_tag>);
static_assert(std::same_as<
              decltype(std::detail::to_type_fold(
                           std::types<int_tag, string_tag, double_tag>{})
                           .type(std::detail::cw<1>))::type,
              string_tag>);
static_assert(std::same_as<
              decltype(std::detail::to_type_fold(
                           std::types<int_tag, string_tag, double_tag>{})
                           .type(std::detail::cw<2>))::type,
              double_tag>);

int main()
{
    auto env =
        std::env(std::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

    auto env2 =
        std::env(std::types<int_tag, double_tag>{}, std::tuple(421, 13.1));

    std::cout << std::get<int_tag>(env) << "\n";
    std::cout << std::get<double_tag>(env) << "\n";
    std::cout << "\n";

    {
        auto const & cenv = env;
        std::cout << std::get<int_tag>(env) << "\n";
        std::cout << std::get<double_tag>(env) << "\n";
        std::cout << "\n";
    }

    {
        auto str_env = std::insert<string_tag>(env, std::string("text"));
        std::cout << std::get<string_tag>(str_env) << "\n";
        std::cout << "\n";

        {
            auto no_dbl_env = std::erase<double_tag>(str_env);
            std::cout << std::get<int_tag>(no_dbl_env) << "\n";
            std::cout << std::get<string_tag>(no_dbl_env) << "\n";
            std::cout << "\n";

            {
                auto env3 = std::insert_unique(no_dbl_env, env2);
                std::cout << std::get<int_tag>(env3) << "\n";
                std::cout << std::get<double_tag>(env3) << "\n";
                std::cout << std::get<string_tag>(env3) << "\n";
                std::cout << "\n";
            }
        }

        {
            auto no_dbl_no_str_env = std::erase<double_tag>(str_env);
            std::cout << std::get<int_tag>(no_dbl_no_str_env) << "\n";
            std::cout << "\n";
        }
    }

    return 0;
}
