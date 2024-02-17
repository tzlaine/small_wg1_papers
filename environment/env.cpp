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

    template<type_list Tags, detail::is_tuple Tuple>
    requires detail::same_arity<Tags, Tuple>
    struct env;

    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> & env);

    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const & env);

    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> && env);

    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const && env);

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

    // TODO: Do we want this, to disable auto-invoking nullary invocables?
    template<typename T>
    struct dont_invoke
    {
        T value;
    };

    template<type_list Tags, detail::is_tuple Tuple>
    requires detail::same_arity<Tags, Tuple>
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

        template<typename Tag>
        static constexpr bool contains(Tag)
        {
            return in_type_list<Tag, Tags>;
        }

        template<template<class...> class TypeList, typename... Tags2>
        requires type_list<TypeList<Tags2...>>
        static constexpr bool contains_all_of(TypeList<Tags2...>)
        {
            return (in_type_list<Tags2, Tags> && ...);
        }

        // TODO: Add invocation of result if its a nullary function.  Should
        // this go here, or get()?
#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag)
        {
            return stdexec::get<Tag>((Self &&) self);
        }
#else
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag) &
        {
            return stdexec::get<Tag>(*this);
        }
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag) const &
        {
            return stdexec::get<Tag>(*this);
        }
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag) &&
        {
            return stdexec::get<Tag>(std::move(*this));
        }
        template<in_type_list<Tags> Tag>
        constexpr decltype(auto) operator[](Tag) const &&
        {
            return stdexec::get<Tag>(std::move(*this));
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

    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> & env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const & env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(env.values);
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> && env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(std::move(env.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr decltype(auto) get(env<Tags, Tuple> const && env)
    {
        constexpr size_t i = detail::index_from_tag<Tag>(Tags{});
        return std::get<i>(std::move(env.values));
    }

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

    template<typename Tag, typename Tags, typename Tuple, typename T>
    requires(!in_type_list<Tag, Tags>)
    constexpr auto insert(env<Tags, Tuple> const & env_, Tag, T && x)
    {
        return env(
            detail::tl_append<std::remove_cvref_t<Tag>>(Tags{}),
            tuple_cat(env_.values, std::tuple((T &&) x)));
    }
    template<typename Tag, typename Tags, typename Tuple, typename T>
    requires(!in_type_list<Tag, Tags>)
    constexpr auto insert(env<Tags, Tuple> && env_, Tag, T && x)
    {
        return env(
            detail::tl_append<Tag>(Tags{}),
            tuple_cat(std::move(env_.values), std::tuple((T &&) x)));
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<typename...>
      typename TypeList,
      typename... Tags2>
    requires type_list<TypeList<Tags2...>> &&
        (in_type_list<Tags2, Tags> && ...)
    constexpr decltype(auto)
    subset(env<Tags, Tuple> const& env_, TypeList<Tags2...> tags2)
    // clang-format on
    {
        return env(tags2, std::tuple(env_[Tags2{}]...));
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<typename...>
      typename TypeList,
      typename... Tags2>
    requires type_list<TypeList<Tags2...>> &&
        (in_type_list<Tags2, Tags> && ...)
    constexpr decltype(auto)
    subset(env<Tags, Tuple>&& env_, TypeList<Tags2...> tags2)
    // clang-format on
    {
        return env(tags2, std::tuple(std::move(env_)[Tags2{}]...));
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (in_type_list<Tags2, Tags> && ...)
    constexpr decltype(auto) subset(env<Tags, Tuple> const & env_, Tags2...)
    // clang-format on
    {
        return stdexec::subset(env_, detail::tl_like(Tags{}, Tags2{}...));
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (in_type_list<Tags2, Tags> && ...)
    constexpr decltype(auto) subset(env<Tags, Tuple> && env_, Tags2...)
    // clang-format on
    {
        return stdexec::subset(
            std::move(env_), detail::tl_like(Tags{}, Tags2{}...));
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
            std::integer_sequence<int, Is...>)
        {
            return std::tuple(
                stdexec::get<OldTags>(env1)..., std::get<Is>(env2.values)...);
        }
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
            env<Tags1, Tuple1> && env1,
            TypeList<OldTags...> old_tags,
            env<Tags2, Tuple2> const & env2,
            std::integer_sequence<int, Is...>)
        {
            return std::tuple(
                stdexec::get<OldTags>(std::move(env1))...,
                std::get<Is>(env2.values)...);
        }
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
            env<Tags2, Tuple2> && env2,
            std::integer_sequence<int, Is...>)
        {
            return std::tuple(
                stdexec::get<OldTags>(env1)...,
                std::get<Is>(std::move(env2.values))...);
        }
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
            env<Tags1, Tuple1> && env1,
            TypeList<OldTags...> old_tags,
            env<Tags2, Tuple2> && env2,
            std::integer_sequence<int, Is...>)
        {
            return std::tuple(
                stdexec::get<OldTags>(std::move(env1))...,
                std::get<Is>(std::move(env2.values))...);
        }
    }

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert(env<Tags1, Tuple1> const & env1, env<Tags2, Tuple2> const & env2)
    {
        constexpr auto old_tags = detail::tl_set_diff(Tags1{}, Tags2{});
        return env(
            detail::tl_cat(old_tags, Tags2{}),
            detail::make_env_tuple(
                env1,
                old_tags,
                env2,
                std::make_integer_sequence<int, std::tuple_size_v<Tuple2>>{}));
    }

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert(env<Tags1, Tuple1> && env1, env<Tags2, Tuple2> const & env2)
    {
        constexpr auto old_tags = detail::tl_set_diff(Tags1{}, Tags2{});
        return env(
            detail::tl_cat(old_tags, Tags2{}),
            detail::make_env_tuple(
                std::move(env1),
                old_tags,
                env2,
                std::make_integer_sequence<int, std::tuple_size_v<Tuple2>>{}));
    }

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert(env<Tags1, Tuple1> const & env1, env<Tags2, Tuple2> && env2)
    {
        constexpr auto old_tags = detail::tl_set_diff(Tags1{}, Tags2{});
        return env(
            detail::tl_cat(old_tags, Tags2{}),
            detail::make_env_tuple(
                env1,
                old_tags,
                std::move(env2),
                std::make_integer_sequence<int, std::tuple_size_v<Tuple2>>{}));
    }

    template<typename Tags1, typename Tuple1, typename Tags2, typename Tuple2>
    constexpr auto
    insert(env<Tags1, Tuple1> && env1, env<Tags2, Tuple2> && env2)
    {
        constexpr auto old_tags = detail::tl_set_diff(Tags1{}, Tags2{});
        return env(
            detail::tl_cat(old_tags, Tags2{}),
            detail::make_env_tuple(
                std::move(env1),
                old_tags,
                std::move(env2),
                std::make_integer_sequence<int, std::tuple_size_v<Tuple2>>{}));
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
            std::integer_sequence<int, Is...>,
            env<Tags2, Tuple2> const & env2,
            TypeList<NewTags...> new_tags)
        {
            return std::tuple(
                std::get<Is>(env1.values)..., stdexec::get<NewTags>(env2)...);
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
            std::integer_sequence<int, Is...>,
            env<Tags2, Tuple2> const & env2,
            TypeList<NewTags...> new_tags)
        {
            return std::tuple(
                std::get<Is>(std::move(env1.values))...,
                stdexec::get<NewTags>(env2)...);
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
            std::integer_sequence<int, Is...>,
            env<Tags2, Tuple2> && env2,
            TypeList<NewTags...> new_tags)
        {
            return std::tuple(
                std::get<Is>(env1.values)...,
                stdexec::get<NewTags>(std::move(env2))...);
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
            std::integer_sequence<int, Is...>,
            env<Tags2, Tuple2> && env2,
            TypeList<NewTags...> new_tags)
        {
            return std::tuple(
                std::get<Is>(std::move(env1.values))...,
                stdexec::get<NewTags>(std::move(env2))...);
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
                std::make_integer_sequence<int, std::tuple_size_v<Tuple1>>{},
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
                std::make_integer_sequence<int, std::tuple_size_v<Tuple1>>{},
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
                std::make_integer_sequence<int, std::tuple_size_v<Tuple1>>{},
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
                std::make_integer_sequence<int, std::tuple_size_v<Tuple1>>{},
                std::move(env2),
                new_tags));
    }

    // TODO: Make all these functions take an environment, not an env.

    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr auto erase(env<Tags, Tuple> const & env_, Tag)
    {
        constexpr auto folded = detail::to_type_fold(Tags{});
        constexpr int i = folded.index(detail::wrap<Tag>());
        return env(
            detail::tl_without_i<i>(folded, Tags{}),
            detail::tuple_without_i<i>(env_.values));
    }
    template<typename Tag, typename Tags, typename Tuple>
    requires in_type_list<Tag, Tags>
    constexpr auto erase(env<Tags, Tuple> && env_, Tag)
    {
        constexpr auto folded = detail::to_type_fold(Tags{});
        constexpr int i = folded.index(detail::wrap<Tag>());
        return env(
            detail::tl_without_i<i>(folded, Tags{}),
            detail::tuple_without_i<i>(std::move(env_.values)));
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<class...>
      class TypeList,
      typename... Tags2>
    requires (in_type_list<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> const & env_, TypeList<Tags2...>)
    // clang-format on
    {
        constexpr auto remaining_tags =
            detail::tl_set_diff(Tags{}, types<Tags2...>{});
        return stdexec::subset(env_, remaining_tags);
    }

    // clang-format off
    template<
      typename Tags,
      typename Tuple,
      template<class...>
      class TypeList,
      typename... Tags2>
    requires (in_type_list<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> && env_, TypeList<Tags2...>)
    // clang-format on
    {
        constexpr auto remaining_tags =
            detail::tl_set_diff(Tags{}, types<Tags2...>{});
        return stdexec::subset(std::move(env_), remaining_tags);
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (in_type_list<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> const & env_, Tags2...)
    // clang-format on
    {
        return stdexec::erase(env_, types<Tags2...>{});
    }

    // clang-format off
    template<typename Tags, typename Tuple, typename... Tags2>
    requires (in_type_list<Tags2, Tags> && ...)
    constexpr auto erase(env<Tags, Tuple> && env_, Tags2...)
    // clang-format on
    {
        return stdexec::erase(std::move(env_), types<Tags2...>{});
    }



    // TODO: Add a value_type_env()?  It would convert a ref_env R to R.base_,
    // convert reference_wrapper<T> members of a ref to T,
    // std::string_view->string, etc.



    // second-order envs

    // TODO: Tests for these.

    template<environment E>
    struct ref_env
    {
        using tag_types = E::tag_types;

        ref_env() = default;
        explicit ref_env(E & base) : base_(std::addressof(base)) {}
        ref_env(std::reference_wrapper<E> ref) : ref_env(*ref) {}

        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t)
        {
            return (*base_)[t];
        }

        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) const
        {
            return std::as_const(*base_)[t];
        }

    private:
        E * base_;
    };

    // TODO: Needed?
    template<environment E>
    ref_env(std::reference_wrapper<E>) -> ref_env<E>;

    template<environment E1, environment E2>
    struct layer_env
    {
        using tag_types = decltype(detail::tl_cat(
            typename E1::tag_types{},
            detail::tl_set_diff(
                typename E2::tag_types{}, typename E1::tag_types{})));

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
        template<typename Self, in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag t)
        {
            if constexpr (in_type_list<Tag, typename E1::tag_types>)
                return ((Self &&) self).base_1_[t];
            else
                return ((Self &&) self).base_2_[t];
        }
#else
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) &
        {
            if constexpr (in_type_list<Tag, typename E1::tag_types>)
                return base_1_[t];
            else
                return base_2_[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) const &
        {
            if constexpr (in_type_list<Tag, typename E1::tag_types>)
                return base_1_[t];
            else
                return base_2_[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) &&
        {
            if constexpr (in_type_list<Tag, typename E1::tag_types>)
                return std::move(base_1_)[t];
            else
                return std::move(base_2_)[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) const &&
        {
            if constexpr (in_type_list<Tag, typename E1::tag_types>)
                return std::move(base_1_)[t];
            else
                return std::move(base_2_)[t];
        }
#endif

    private:
        E1 base_1_;
        E2 base_2_;
    };

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

        struct layer_impl : std::ranges::range_adaptor_closure<layer_impl>
        {
            template<env_or_ref E1, env_or_ref E2>
            requires can_layer_env<E1, E2>
            constexpr auto operator()(E1 && e1, E2 && e2) const
            {
                return layer_env((E1 &&) e1, (E2 &&) e2);
            }
        };
    }

    inline constexpr detail::layer_impl layer;

    // clang-format off
    template<environment E, type_list Tags>
    requires(E::contains_all_of(Tags{}))
    struct filter_env
    // clang-format on
    {
        using tag_types = Tags;

        // clang-format off
        filter_env() requires std::default_initializable<E> = default;
        // clang-format on
        filter_env(E base, Tags tags) : base_(std::move(base_)) {}

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag t)
        {
            return ((Self &&) self).base_[t];
        }
#else
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) &
        {
            return base_[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) const &
        {
            return base_[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) &&
        {
            return std::move(base_)[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) const &&
        {
            return std::move(base_)[t];
        }
#endif

    private:
        E base_;
    };

    namespace detail {
        // clang-format off
        template<typename T, typename Tags>
        concept can_filter_env = requires {
            filter_env(std::declval<T>(), Tags{});
        };
        // clang-format on

        struct filter_impl : std::ranges::range_adaptor_closure<layer_impl>
        {
            template<env_or_ref E, type_list Tags>
            requires can_filter_env<E, Tags>
            constexpr auto operator()(E && e, Tags tags) const
            {
                return filter_env((E &&) e, tags);
            }
        };
    }

    inline constexpr detail::filter_impl filter;

    template<environment E, type_list Tags>
    struct without_env
    {
        using tag_types =
            decltype(detail::tl_set_diff(typename E::tag_types{}, Tags{}));

        // clang-format off
        without_env() requires std::default_initializable<E> = default;
        // clang-format on
        without_env(E base, Tags tags) : base_(std::move(base_)) {}

#if defined(__cpp_explicit_this_parameter)
        template<typename Self, in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](this Self && self, Tag t)
        {
            return ((Self &&) self).base_[t];
        }
#else
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) &
        {
            return base_[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) const &
        {
            return base_[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) &&
        {
            return std::move(base_)[t];
        }
        template<in_type_list<tag_types> Tag>
        constexpr decltype(auto) operator[](Tag t) const &&
        {
            return std::move(base_)[t];
        }
#endif

    private:
        E base_;
    };

    namespace detail {
        // clang-format off
        template<typename T, typename Tags>
        concept can_without_env = requires {
            without_env(std::declval<T>(), Tags{});
        };
        // clang-format on

        struct without_impl : std::ranges::range_adaptor_closure<layer_impl>
        {
            template<env_or_ref E, type_list Tags>
            requires can_without_env<E, Tags>
            constexpr auto operator()(E && e, Tags tags) const
            {
                return without_env((E &&) e, tags);
            }
        };
    }

    inline constexpr detail::without_impl without;
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

    // TODO: Test with other types.
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

        EXPECT_TRUE(env.contains(int_tag{}));
        EXPECT_TRUE(env.contains(double_tag{}));
        EXPECT_TRUE(env.contains(string_tag{}));
        EXPECT_FALSE(env.contains(int{}));

        EXPECT_TRUE(env.contains_all_of(stdexec::types<int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(stdexec::types<double_tag>{}));
        EXPECT_TRUE(env.contains_all_of(stdexec::types<string_tag>{}));

        EXPECT_TRUE(env.contains_all_of(stdexec::types<string_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(stdexec::types<double_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(
            stdexec::types<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(env.contains_all_of(stdexec::types<int_2_tag>{}));
        EXPECT_FALSE(env.contains_all_of(stdexec::types<int_tag, int_2_tag>{}));

#if HAVE_BOOST_MP11
        EXPECT_TRUE(env.contains_all_of(mp_list<int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(mp_list<double_tag>{}));
        EXPECT_TRUE(env.contains_all_of(mp_list<string_tag>{}));

        EXPECT_TRUE(env.contains_all_of(mp_list<string_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(mp_list<double_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(
            mp_list<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(env.contains_all_of(mp_list<int_2_tag>{}));
        EXPECT_FALSE(env.contains_all_of(mp_list<int_tag, int_2_tag>{}));
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

        EXPECT_TRUE(env.contains(int_tag{}));
        EXPECT_TRUE(env.contains(double_tag{}));
        EXPECT_TRUE(env.contains(string_tag{}));
        EXPECT_FALSE(env.contains(int{}));

        EXPECT_TRUE(env.contains_all_of(mp_list<int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(mp_list<double_tag>{}));
        EXPECT_TRUE(env.contains_all_of(mp_list<string_tag>{}));

        EXPECT_TRUE(env.contains_all_of(mp_list<string_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(mp_list<double_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(
            mp_list<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(env.contains_all_of(mp_list<int_2_tag>{}));
        EXPECT_FALSE(env.contains_all_of(mp_list<int_tag, int_2_tag>{}));

        EXPECT_TRUE(env.contains_all_of(stdexec::types<int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(stdexec::types<double_tag>{}));
        EXPECT_TRUE(env.contains_all_of(stdexec::types<string_tag>{}));

        EXPECT_TRUE(env.contains_all_of(stdexec::types<string_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(stdexec::types<double_tag, int_tag>{}));
        EXPECT_TRUE(env.contains_all_of(
            stdexec::types<string_tag, double_tag, int_tag>{}));

        EXPECT_FALSE(env.contains_all_of(stdexec::types<int_2_tag>{}));
        EXPECT_FALSE(env.contains_all_of(stdexec::types<int_tag, int_2_tag>{}));

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

#endif
