#include <iostream>
#include <ranges>
#include <string>
#include <tuple>
#include <utility>

#include <gtest/gtest.h>


// TODO: Do we want anything to associate a tag to a particular type? The
// code below lets sttring_tag be the tag type for a std::complex if you
// like....

namespace std {
    namespace detail {
        template<typename T>
        constexpr bool has_type_params = false;
        template<template<class...> class Template, typename... Ts>
        constexpr bool has_type_params<Template<Ts...>> = true;

        // TODO: What kinds of types can be in the env?
        template<typename T>
        concept env_value = same_as<decay_t<T>, T>;

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

        struct temp_fold_base;
        template<
            typename Tag,
            typename T,
            int I = 0,
            typename Tail = temp_fold_base>
        struct temp_fold;
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
                return temp_fold<Tag, T>{std::move(*this), std::move(x)};
            }
        };

        template<typename TempFolded, typename Tag>
        concept new_tag = (!TempFolded::contains(wrap<Tag>()));

        template<typename Tag, typename T, int I, typename Tail>
        struct temp_fold : Tail
        {
            temp_fold(Tail && tail, T && x) :
                Tail(std::move(tail)), x_(std::move(x))
            {}
            consteval wrapper<Tag> tag_type(constant_wrapper<I>) const
            {
                return {};
            }
            constexpr T value(constant_wrapper<I>) && { return x_; }
            static consteval bool contains(wrapper<Tag>) { return true; }

            template<typename Tag2, typename T2>
            constexpr auto operator()(Tag2, T2 && x)
            requires new_tag<temp_fold, Tag2>
            {
                return temp_fold<Tag2, T2, I + 1, temp_fold>{
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
        is_empty_v<T> && semiregular<T> && detail::has_type_params<T>;

    template<typename T>
    concept env_tuple = detail::tuple_args_env_vals<T>;

    // clang-format off
    template<typename T>
    concept queryable_environment = requires(T x) {
      { x.tags } -> type_list;
      { x.values } -> env_tuple;
    };
    // TODO: Support member functions returning same? Maybe free functions
    // too?

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

        template<typename Tuple, int... Is>
        constexpr auto sub_tuple(Tuple && t, integer_sequence<int, Is...>)
        {
            return tuple(std::get<Is>((Tuple &&) t)...);
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
        template<int I, typename Tuple>
        constexpr auto tuple_without_i(Tuple && t)
        {
            return sub_tuple(
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
            typename... Ts,
            template<class...>
            class TypeList,
            typename... Tags>
        constexpr bool
        same_arity_impl(TypeList<Tags...> tl, wrapper<tuple<Ts...>> w)
        {
            return sizeof...(Ts) == sizeof...(Tags);
        }
        template<typename Tags, typename Tuple>
        constexpr bool
            same_arity = detail::same_arity_impl(Tags{}, wrap<Tuple>());
    }

    template<typename T>
    concept queryable =
        detail::free_queryable<T> || detail::member_queryable<T>;

    template<typename... Ts>
    struct types
    {};

    template<type_list Tags, env_tuple Tuple>
    requires detail::same_arity<Tags, Tuple>
    struct env;

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> & env);

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const & env);

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> && env);

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const && env);

    namespace detail {
        template<typename Tag, typename T, int I, typename Tail, int... Is>
        auto temp_fold_tags(
            temp_fold<Tag, T, I, Tail> const & folded,
            integer_sequence<int, Is...>)
        {
            return types<typename decltype(folded.tag_type(cw<Is>))::type...>{};
        }

        template<typename Tag, typename T, int I, typename Tail, int... Is>
        auto temp_fold_values(
            temp_fold<Tag, T, I, Tail> && folded, integer_sequence<int, Is...>)
        {
            return tuple(std::move(folded).value(cw<Is>)...);
        }
    }

    template<type_list Tags, env_tuple Tuple>
    requires detail::same_arity<Tags, Tuple>
    struct env
    {
        using tags_type = Tags;
        using tuple_type = Tuple;

        // clang-format off
        constexpr env()
            requires default_initializable<Tuple> = default;
        constexpr env(env const & other)
            requires copy_constructible<Tuple> = default;
        constexpr env(env && other)
            requires move_constructible<Tuple> = default;
        constexpr env & operator=(env const & other)
            requires assignable_from<Tuple &, Tuple const &> = default;
        constexpr env & operator=(env && other)
            requires assignable_from<Tuple &, Tuple &&> = default;
        // clang-format on

        constexpr env(Tags tags, Tuple const & values) :
            tags(tags), values(values)
        {}
        constexpr env(Tags tags, Tuple && values) :
            tags(tags), values(std::move(values))
        {}

        template<typename Tag, typename T, int I, typename Tail>
        constexpr env(detail::temp_fold<Tag, T, I, Tail> && folded) :
            tags(detail::temp_fold_tags(
                folded, make_integer_sequence<int, I + 1>{})),
            values(detail::temp_fold_values(
                std::move(folded), make_integer_sequence<int, I + 1>{}))
        {}

        constexpr bool operator==(env const & other) const
        {
            return values == other.values;
        }

        template<typename Tag>
        constexpr bool contains(Tag) const
        {
            return detail::has_tag<Tag, Tags>;
        }

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, typename Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag)
        {
            return std::get<Tag>((Self &&) * this);
        }
#else
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
            return std::get<Tag>(std::move(*this));
        }
        template<typename Tag>
        constexpr decltype(auto) operator[](Tag) const &&
        {
            return std::get<Tag>(std::move(*this));
        }
#endif

        Tags tags;
        Tuple values;
    };

    template<typename Tag, typename T, int I, typename Tail>
    env(detail::temp_fold<Tag, T, I, Tail> && folded) -> env<
        decltype(detail::temp_fold_tags(
            std::declval<detail::temp_fold<Tag, T, I, Tail>>(),
            make_integer_sequence<int, I + 1>{})),
        decltype(detail::temp_fold_values(
            std::declval<detail::temp_fold<Tag, T, I, Tail>>(),
            make_integer_sequence<int, I + 1>{}))>;

    inline constexpr env<types<>, tuple<>> empty_env;

    inline detail::temp_fold_base make_env;

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> & env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const & env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> && env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(std::move(env.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const && env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(std::move(env.values));
    }

    template<size_t I, typename Tags, typename Tuple>
    requires(I < tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> & env)
    {
        return std::get<I>(env.values);
    }
    template<size_t I, typename Tags, typename Tuple>
    requires(I < tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> const & env)
    {
        return std::get<I>(env.values);
    }
    template<size_t I, typename Tags, typename Tuple>
    requires(I < tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> && env)
    {
        return std::get<I>(std::move(env.values));
    }
    template<size_t I, typename Tags, typename Tuple>
    requires(I < tuple_size_v<Tuple>) constexpr decltype(auto)
        get(env<Tags, Tuple> const && env)
    {
        return std::get<I>(std::move(env.values));
    }

    template<typename Tag, typename Tags, typename Tuple, typename T>
    requires(!detail::has_tag<Tag, Tags>) &&
        detail::env_value<T> constexpr auto insert(
            env<Tags, Tuple> const & env_, Tag, T && x)
    {
        return env(
            detail::tl_append<remove_cvref_t<Tag>>(Tags{}),
            tuple_cat(env_.values, tuple((T &&) x)));
    }
    template<typename Tag, typename Tags, typename Tuple, typename T>
    requires(!detail::has_tag<Tag, Tags>) &&
        detail::env_value<T> constexpr auto insert(
            env<Tags, Tuple> && env_, Tag, T && x)
    {
        return env(
            detail::tl_append<Tag>(Tags{}),
            tuple_cat(std::move(env_.values), tuple((T &&) x)));
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<typename...>
      typename TypeList,
      typename... Tags2>
    requires type_list<TypeList<Tags2...>> &&
        (detail::has_tag<Tags2, Tags> && ...)
    constexpr decltype(auto)
    subset(env<Tags, Tuple> const& env_, TypeList<Tags2...> tags2)
    // clang-format on
    {
        return env(tags2, tuple(env_[Tags2{}]...));
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<typename...>
      typename TypeList,
      typename... Tags2>
    requires type_list<TypeList<Tags2...>> &&
        (detail::has_tag<Tags2, Tags> && ...)
    constexpr decltype(auto)
    subset(env<Tags, Tuple>&& env_, TypeList<Tags2...> tags2)
    // clang-format on
    {
        return env(tags2, tuple(std::move(env_)[Tags2{}]...));
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (detail::has_tag<Tags2, Tags> && ...)
    constexpr decltype(auto) subset(env<Tags, Tuple> const & env_, Tags2...)
    // clang-format on
    {
        return subset(env_, types<Tags2...>{});
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (detail::has_tag<Tags2, Tags> && ...)
    constexpr decltype(auto) subset(env<Tags, Tuple> && env_, Tags2...)
    // clang-format on
    {
        return subset(std::move(env_), types<Tags2...>{});
    }

    namespace detail {
        template<
            typename Tags1,
            typename Tuple1,
            template<class...>
            class TypeList,
            typename... OldTags,
            typename Tags2,
            typename Tuple2,
            int... Is>
        constexpr auto make_env_tuple(
            env<Tags1, Tuple1> const & env1,
            TypeList<OldTags...> old_tags,
            env<Tags2, Tuple2> const & env2,
            integer_sequence<int, Is...>)
        {
            return tuple(
                std::get<OldTags>(env1)..., std::get<Is>(env2.values)...);
        }
    }

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert(env<Tags1, Tuple1> const & env1, env<Tags2, Tuple2> const & env2)
    {
        constexpr auto old_tags = detail::tl_set_diff(Tags1{}, Tags2{});
        return env(
            detail::tl_cat(old_tags, Tags1{}),
            detail::make_env_tuple(
                env1,
                old_tags,
                env2,
                make_integer_sequence<int, tuple_size_v<Tuple2>>{}));
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
            env<Tags1, Tuple1> && env1,
            integer_sequence<int, Is...>,
            env<Tags2, Tuple2> const & env2,
            TypeList<NewTags...> new_tags)
        {
            return tuple(
                std::get<Is>(std::move(env1.values))...,
                std::get<NewTags>(env2)...);
        }
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
            env<Tags2, Tuple2> && env2,
            TypeList<NewTags...> new_tags)
        {
            return tuple(
                std::get<Is>(env1.values)...,
                std::get<NewTags>(std::move(env2))...);
        }
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
            env<Tags1, Tuple1> && env1,
            integer_sequence<int, Is...>,
            env<Tags2, Tuple2> && env2,
            TypeList<NewTags...> new_tags)
        {
            return tuple(
                std::get<Is>(std::move(env1.values))...,
                std::get<NewTags>(std::move(env2))...);
        }
    }

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto insert_unique(
        env<Tags1, Tuple1> const & env1, env<Tags2, Tuple2> const & env2)
    {
        constexpr auto new_tags = detail::tl_set_diff(Tags2{}, Tags1{});
        return env(
            detail::tl_cat(Tags1{}, new_tags),
            detail::make_env_tuple(
                env1,
                make_integer_sequence<int, tuple_size_v<Tuple1>>{},
                env2,
                new_tags));
    }
    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert_unique(env<Tags1, Tuple1> const & env1, env<Tags2, Tuple2> && env2)
    {
        constexpr auto new_tags = detail::tl_set_diff(Tags2{}, Tags1{});
        return env(
            detail::tl_cat(Tags1{}, new_tags),
            detail::make_env_tuple(
                env1,
                make_integer_sequence<int, tuple_size_v<Tuple1>>{},
                std::move(env2),
                new_tags));
    }
    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert_unique(env<Tags1, Tuple1> && env1, env<Tags2, Tuple2> const & env2)
    {
        constexpr auto new_tags = detail::tl_set_diff(Tags2{}, Tags1{});
        return env(
            detail::tl_cat(Tags1{}, new_tags),
            detail::make_env_tuple(
                std::move(env1),
                make_integer_sequence<int, tuple_size_v<Tuple1>>{},
                env2,
                new_tags));
    }
    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert_unique(env<Tags1, Tuple1> && env1, env<Tags2, Tuple2> && env2)
    {
        constexpr auto new_tags = detail::tl_set_diff(Tags2{}, Tags1{});
        return env(
            detail::tl_cat(Tags1{}, new_tags),
            detail::make_env_tuple(
                std::move(env1),
                make_integer_sequence<int, tuple_size_v<Tuple1>>{},
                std::move(env2),
                new_tags));
    }

    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr auto erase(env<Tags, Tuple> const & env_, Tag)
    {
        constexpr int i = detail::index_from_tag<Tag>(Tags{});
        return env(
            detail::tl_erase<Tag>(Tags{}),
            detail::tuple_without_i<i>(env_.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires detail::has_tag<Tag, Tags>
    constexpr auto erase(env<Tags, Tuple> && env_, Tag)
    {
        constexpr int i = detail::index_from_tag<Tag>(Tags{});
        return env(
            detail::tl_erase<Tag>(Tags{}),
            detail::tuple_without_i<i>(std::move(env_.values)));
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<class...>
      class TypeList,
      typename... Tags2>
    requires (detail::has_tag<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> const & env_, TypeList<Tags2...>)
    // clang-format on
    {
        constexpr auto remaining_tags =
            detail::tl_set_diff(Tags{}, types<Tags2...>{});
        return subset(env_, remaining_tags);
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<class...>
      class TypeList,
      typename... Tags2>
    requires (detail::has_tag<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> && env_, TypeList<Tags2...>)
    // clang-format on
    {
        constexpr auto remaining_tags =
            detail::tl_set_diff(Tags{}, types<Tags2...>{});
        return subset(std::move(env_), remaining_tags);
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (detail::has_tag<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> const & env_, Tags2...)
    // clang-format on
    {
        return erase(env_, types<Tags2...>{});
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (detail::has_tag<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> && env_, Tags2...)
    // clang-format on
    {
        return erase(std::move(env_), types<Tags2...>{});
    }
}

// Eric Niebler ? Today at 5:02 PM
// for a general purpose utility you really only need a few things: a way to
// create an env from a query and a value, a way to join two envs, and a way
// to remove a query from an env. i have also found useful a
// reference_wrapper-like env and, for p2300, a forwarding env that filters
// out non-forwarding queries.

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

TEST(env_, make_env_)
{
    {
        std::env env = std::make_env(int_tag{}, 42)(double_tag{}, 13.0);

        auto const expected =
            std::env(std::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

        EXPECT_TRUE(env == expected);
    }
    {
        std::env env = std::make_env(int_tag{}, 42)(double_tag{}, 13.0)(
            string_tag{}, std::string("foo"));

        auto const expected = std::env(
            std::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        EXPECT_TRUE(env == expected);
    }
}

TEST(env_, type_get)
{
    auto env = std::env(
        std::types<int_tag, double_tag, string_tag>{},
        std::tuple(42, 13.0, std::string("foo")));

    {
        auto const & c_env = env;

        EXPECT_EQ(std::get<int_tag>(c_env), 42);
        EXPECT_EQ(std::get<double_tag>(c_env), 13.0);
        EXPECT_EQ(std::get<string_tag>(c_env), "foo");

        static_assert(
            std::same_as<decltype(std::get<int_tag>(c_env)), int const &>);
        static_assert(std::same_as<
                      decltype(std::get<string_tag>(c_env)),
                      std::string const &>);
        static_assert(std::same_as<
                      decltype(std::get<int_tag>(std::move(c_env))),
                      int const &&>);
        static_assert(std::same_as<
                      decltype(std::get<string_tag>(std::move(c_env))),
                      std::string const &&>);
    }

    EXPECT_EQ(std::get<int_tag>(env), 42);
    EXPECT_EQ(std::get<double_tag>(env), 13.0);
    EXPECT_EQ(std::get<string_tag>(env), "foo");

    std::get<int_tag>(env) = 8;
    EXPECT_EQ(std::get<int_tag>(env), 8);

    std::get<double_tag>(env) = 19.9;
    EXPECT_EQ(std::get<double_tag>(env), 19.9);

    std::string const moved_to = std::get<string_tag>(std::move(env));
    EXPECT_EQ(moved_to, "foo");
    EXPECT_EQ(std::get<string_tag>(env), "");

    static_assert(std::same_as<decltype(std::get<int_tag>(env)), int &>);
    static_assert(
        std::same_as<decltype(std::get<string_tag>(env)), std::string &>);
    static_assert(
        std::same_as<decltype(std::get<int_tag>(std::move(env))), int &&>);
    static_assert(std::same_as<
                  decltype(std::get<string_tag>(std::move(env))),
                  std::string &&>);
}

TEST(env_, nttp_get)
{
    auto env = std::env(
        std::types<int_tag, double_tag, string_tag>{},
        std::tuple(42, 13.0, std::string("foo")));

    {
        auto const & c_env = env;

        EXPECT_EQ(std::get<0>(env), 42);
        EXPECT_EQ(std::get<1>(env), 13.0);
        EXPECT_EQ(std::get<2>(env), "foo");

        static_assert(std::same_as<decltype(std::get<0>(c_env)), int const &>);
        static_assert(
            std::same_as<decltype(std::get<2>(c_env)), std::string const &>);
        static_assert(
            std::
                same_as<decltype(std::get<0>(std::move(c_env))), int const &&>);
        static_assert(std::same_as<
                      decltype(std::get<2>(std::move(c_env))),
                      std::string const &&>);
    }

    EXPECT_EQ(std::get<0>(env), 42);
    EXPECT_EQ(std::get<1>(env), 13.0);
    EXPECT_EQ(std::get<2>(env), "foo");

    std::get<0>(env) = 8;
    EXPECT_EQ(std::get<0>(env), 8);

    std::get<1>(env) = 19.9;
    EXPECT_EQ(std::get<1>(env), 19.9);

    std::string const moved_to = std::get<2>(std::move(env));
    EXPECT_EQ(moved_to, "foo");
    EXPECT_EQ(std::get<2>(env), "");

    static_assert(std::same_as<decltype(std::get<0>(env)), int &>);
    static_assert(std::same_as<decltype(std::get<2>(env)), std::string &>);
    static_assert(std::same_as<decltype(std::get<0>(std::move(env))), int &&>);
    static_assert(
        std::same_as<decltype(std::get<2>(std::move(env))), std::string &&>);
}

TEST(env_, index_contains_equals)
{
    auto env = std::env(
        std::types<int_tag, double_tag, string_tag>{},
        std::tuple(42, 13.0, std::string("foo")));

    EXPECT_TRUE(env == env);

    {
        auto const & c_env = env;

        EXPECT_TRUE(env == c_env);

        EXPECT_EQ(std::get<int_tag>(env), 42);
        EXPECT_EQ(std::get<double_tag>(env), 13.0);
        EXPECT_EQ(std::get<string_tag>(env), "foo");

        static_assert(std::same_as<decltype(c_env[int_tag{}]), int const &>);
        static_assert(
            std::same_as<decltype(c_env[string_tag{}]), std::string const &>);
        static_assert(
            std::same_as<decltype(std::move(c_env)[int_tag{}]), int const &&>);
        static_assert(std::same_as<
                      decltype(std::move(c_env)[string_tag{}]),
                      std::string const &&>);
    }

    EXPECT_TRUE(env.contains(int_tag{}));
    EXPECT_TRUE(env.contains(double_tag{}));
    EXPECT_TRUE(env.contains(string_tag{}));
    EXPECT_FALSE(env.contains(int{}));

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
    static_assert(std::same_as<decltype(std::move(env)[int_tag{}]), int &&>);
    static_assert(
        std::same_as<decltype(std::move(env)[string_tag{}]), std::string &&>);
}

TEST(env_, single_insert)
{
    auto env =
        std::env(std::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

    {
        auto const expected = std::env(
            std::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string("foo")));

        auto inserted = std::insert(env, string_tag{}, std::string("foo"));
        EXPECT_TRUE(inserted == expected);

        auto const expected_2 = std::env(
            std::types<int_tag, double_tag, string_tag, string_2_tag>{},
            std::tuple(42, 13.0, std::string("foo"), std::string("bar")));

        auto inserted_2 = std::insert(
            std::move(inserted), string_2_tag{}, std::string("bar"));
        EXPECT_TRUE(inserted_2 == expected_2);

        auto const inserted_expected_after_move = std::env(
            std::types<int_tag, double_tag, string_tag>{},
            std::tuple(42, 13.0, std::string()));
        EXPECT_TRUE(inserted == inserted_expected_after_move);
    }
}

TEST(env_, subset) {}

TEST(env_, insert_env) {}

TEST(env_, insert_unique) {}

TEST(env_, single_erase)
{
    auto env = std::env(
        std::types<int_tag, double_tag, string_tag>{},
        std::tuple(42, 13.0, std::string("foo")));

    {
        auto const expected =
            std::env(std::types<int_tag, double_tag>{}, std::tuple(42, 13.0));

        auto erased = std::erase(env, string_tag{});
        EXPECT_TRUE(erased == expected);
    }

    {
        auto const expected = std::env(
            std::types<int_tag, string_tag>{},
            std::tuple(42, std::string("foo")));

        auto erased = std::erase(env, double_tag{});
        EXPECT_TRUE(erased == expected);
    }

    {
        auto const expected = std::env(
            std::types<double_tag, string_tag>{},
            std::tuple(13.0, std::string("foo")));

        auto erased = std::erase(env, int_tag{});
        EXPECT_TRUE(erased == expected);
    }

    {
        auto erased_1 = std::erase(env, int_tag{});
        auto erased_2 = std::erase(erased_1, double_tag{});
        auto final_ = std::erase(erased_2, string_tag{});
        EXPECT_TRUE(final_ == std::empty_env);
    }
}

TEST(env_, multi_erase)
{
    auto const initial_env = std::env(
        std::types<int_tag, double_tag, string_tag>{},
        std::tuple(42, 13.0, std::string("foo")));

    {
        auto env = initial_env;

        auto const expected =
            std::env(std::types<string_tag>{}, std::tuple(std::string("foo")));

        auto erased = std::erase(env, int_tag{}, double_tag{});
        EXPECT_TRUE(erased == expected);
    }

    {
        auto env = initial_env;

        auto const expected =
            std::env(std::types<string_tag>{}, std::tuple(std::string("foo")));

        auto erased = std::erase(std::move(env), double_tag{}, int_tag{});
        EXPECT_TRUE(erased == expected);

        EXPECT_EQ(env[string_tag{}], std::string());
    }

    {
        auto env = initial_env;

        auto const expected =
            std::env(std::types<double_tag>{}, std::tuple(13.0));

        auto erased = std::erase(env, int_tag{}, string_tag{});
        EXPECT_TRUE(erased == expected);
    }

    {
        auto env = initial_env;

        auto const expected =
            std::env(std::types<string_tag>{}, std::tuple(std::string("foo")));

        auto erased = std::erase(env, string_tag{}, double_tag{}, int_tag{});
        EXPECT_TRUE(erased == std::empty_env);
    }
}
