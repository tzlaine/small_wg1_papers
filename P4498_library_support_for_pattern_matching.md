---
title: "Library Support for Pattern Matching"
document: D4498R0
date: today
audience:
  - LEWG
  - EWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
  - name: Michael Park
    email: <mcypark@gmail.com>
toc: true
monofont: "DejaVu Sans Mono"

---

# Changelog

Initial revision

# Motivation

[@P2688R6]{.title} introduces a user-specializable template known to the
compiler, `std::alternative_traits`.  `std::alternative_traits` allows the
user to specify how variant-like types (sum types) are decomposable within
`match` expressions and statements.  Here is an example of a `match` statement
on a simple variant value:

::: cmptable
### P2688
```cpp
std::variant<int, std::string> val;

match (val) {
  case { int i }         => use(i);
  case { std::string s } => use(s);
}
```

### C++26 Equivalent
```cpp
std::variant<int, std::string> val;

std::visit([](auto && x) { use(x); }, val);
```
:::

The compiler has no idea what a `std::variant` is, of course.  A
specialization of `std::alternative_traits` relays the semantics of
`std::variant`, as they relate to pattern matching, to the compiler.  This
paper defines the properties of `std::alternative_traits` that the user may
specialize in order to communicate the desired semantics of a particular
variant-like type or template to the compiler.

# Design Goals

There are a number of different kinds of variant-like types and templates.
Some of the ones in the standard library are: `std::optional<T>`,
`std::expected<T, E>`, `std::variant<Ts...>`, `std::any`, and
`std::exception_ptr`.  There is a fair amount of difference among these
different types/templates, and we need `std::alternative_traits` to accommodate
all of them if we want a fully general language feature.

## Closed vs. Open Types

A particular specialization of `std::variant` can only hold a value of one of
a fixed (and thus closed) set of types.  A `std::any` can hold a value of any
copyable type; the set of types it can contain is therefore open.

Matching a subject `S` of a closed variant-like type allows the compiler to
reason about viability and exhaustiveness of the matchers applied to `S`.

```cpp
std::variant<int, std::string> val;

match (val) {
  case { int i }         => use(i); // ok
  case { std::string s } => use(s); // ok
  case { char c }        => use(c); // error: not useful; val can never contain a char
}

match (val) {
  case { int i }         => use(i); // ok
} // error: not exhaustive; the possibility that val is a std::string is not handled
```

With open variant-like types, there is no candidate list of types that must be
handled.  Rather, a particular type must be tried, via cast, as with
`std::any_cast()`.  The pattern matching language feature handles the cast; it
is not explicitly written.  It does this by using members of
`std::alternative_traits`.

```cpp
std::any val;

match (val) {
  case { int: auto& integer }      => use(integer); // any_cast-ing val to int works
  case { const std::string& text } => print(text);  // any_cast-ing val to std::string works
  case _                           => unknown();    // val contains something else
}
```

## Nullable Types

`std::any` is not only an example of an open variant-like type, but it has an
additional property that is common in type erasure -- it may be empty
altogether.  `std::optional` is also possibly empty.

## Binary vs. N-ary Closed Types

`std::expected` has only two states: the value-state and the error-state.
`std::variant` can be specialized with any number of template arguments --
this includes two.  There is no compelling reason to treat a binary closed
type any different than a `std::variant` with two alternatives.  As such, the
pattern matching language feature does not have any special handling for
closed variant-like types with exactly two states.  Said another way, all
closed variant-like types are treated as N-ary closed variant-like types, even
when N=2.

Note that many closed variant-like types have two states, one of which is its
null state; `std::optional` is one such example.

## Residual States

For a closed variant-like type `C`, there may be one or more oddball states
that do not fit in the closed set of types associated with `C`.  For instance,
`std::variant<int, std::string>` can be in one of three states: it may contain
an `int`, it may contain a `std::string`, or it may be
`.valueless_by_exception()`.

`std::expected` does not have a residual state, and so after we have
exhaustively handled all possible states, also adding the catchall/wildcard
matcher `_` may produce a warning.  The compiler knows that `_` is not doing
anything useful.

```cpp
void match_expected(std::expected<int, std::string> result) {
  match (result) {
    case { int value } => use(value);
    case { std::string error } => report(error);
    case _ => ; // warning: redundant case
  }
}
```

With a `std::variant`, no such warning applies, because of
`.valueless_by_exception()`.

```cpp
void match_variant(std::variant<int, std::string> val) {
  match (val) {
    case { int i } => use(i);
    case { std::string s } => use(s);
    case _ => ; // may cover a residual state
  }
}
```

## Named and Indexed Selectors

It is often impossible to differentiate between two states of a variant-like
type based on the type of the states alone.  For instance, a very common
specialization of `std::expected` is `std::expected<std::string,
std::string>`.  In this case, what do you write as your matchers?  This
obviously does not work:

```cpp
std::expected<std::string, std::string> result;
match (result) {
  case { std::string contents } => process(contents); // ok, but will catch both the
                                                      // value case and the error case
  case { std::string message }  => report(message);   // error: duplicate case
}
```

Removing the second case will "work", in that it's well-formed code, but it
leaves the code treating the value and error cases identically.

To deal with this, we need some way of naming the semantic states of some
variant-like types, in order to refer to them consistently, even if they have
the same type.  Instead of the `match` above, we could have written:

```cpp
std::expected<std::string, std::string> result;

match (result) {
  case { .value: std::string contents } => process(contents); // using named selector .value
  case { .error: std::string message }  => report(message);   // using named selector .error
}
```

This explicitly names which kind of state we're referring to, regardless of
type.  Since the named selectors do not mention type, they cannot be used to
form a declaration, and so they must be accompanied by the declarations
written after the `:` above.

Additionally, some users will want to refer to the nth type in a closed
variant-like type.  [@P2688R6]{.title} supports that as well.

```cpp
std::variant<std::string, std::string> val;

match (val) {
  case { .index<0> } => std::print("first");  // using indexed selector 0
  case { .index<1> } => std::print("second"); // using indexed selector 1
}
```

## Types with a Closed Set of Possible Values

Some types are not actually variant at all, but objects of that type only come
in a fixed number of possible values.  An enumeration is an example of this.
So is `std::chrono::weekday`, which can only have only have the values
`std::chrono::Sunday`, `std::chrono::Monday`, `std::chrono::Tuesday`, etc.

```cpp
std::chrono::weekday day;

match (day) {
case std::chrono::Saturday => std::print("Why are you at work?");
case std::chrono::Sunday   => std::print("Go home!");
case _ => ; // carry on
}
```

Support for enumerations is built into the pattern matching language feature.
Support for a type like `std::chrono::weekday` must be added by its author.
It would make no sense to try to shoehorn a type like `std::chrono::weekday`
into the form of an enumeration, but it has enough in common with one that it
becomes useful to match on it in a similar way, as above.

## Design Goals Summary

Based on the sections above, we need a `std::alternative_traits` that:

- supports listing the closed set of types for the closed case, and providing
an interface to cast-like behavior in the open case;

- can represent nullability as a state;

- treats nullable and other binary closed variant-like types just like it
would a `std::variant` with two states;

- can represent residual states;

- can be used to specify named and indexed selectors; and

- supports enumeration-like, nonintegral types.


# Design

`std::alternative_traits` is a struct in `std` that exists to be specialized
by authors of variant-like types and templates:

```cpp
template <class T>
struct alternative_traits;
```

`std::alternative_info` is a helper type that represents a particular
alternative state in a specialization of `std::alternative_traits`.

```cpp
struct alternative_info {
  consteval alternative_info() = default;
  consteval alternative_info(meta::info info = {}, bool empty = false);

  meta::info info;
  bool empty;
}
```

`info` is a reflection of the state; the state may be a type or a constant.  A
constant applies to a type like `std::chrono::weekday`, which must always be
one of a closed set of values.

`empty` indicates whether this state is the empty state (e.g. `std::nullopt`).

`std::alternative_traits` has two forms: one for closed types, and another for
open types.

## The Closed Protocol

Here is an example of a specialization of `std::alternative_traits` for
`std::expected`, using the closed protocol:

```cpp
template <class T, class E>
struct alternative_traits<expected<T, E>> {
  static constexpr alternative_info alternatives[] = {^^T, ^^E};
  static constexpr bool has_residual_states = false;

  enum class state : bool { value = false, error = true };

  static constexpr state index(const expected<T, E>& value) noexcept {
    return value.has_value() ? state::value : state::error;
  }

  template <state State, class Self>
  static constexpr decltype(auto) get(Self&& self) {
    return match constexpr (State) -> decltype(auto) {
      case state::value => *std::forward<Self>(self);
      case state::error => std::forward<Self>(self).error();
    };
  }
};
```

`alternatives` is the closed list of the states of `std::expected`.  In this
case, each reflection is a type.  Other reflections might be values, like
`std::nullopt`.

`has_residual_states` is false, because `std::expected` is always either in
the value state or the error state; there are no "oddball" states.

`state` represents the possible states of a `std::expected` in the form of an
enumeration.  It is used in `index()` and `get()`.  It also provides the named
selectors `.value` and `.error`.  Named and indexed selectors for a type `C`
are found by the compiler by looking within
`std::alternative_traits<C>::state`.  Any names found there become selectors.
If we had written this for `state` instead:

```cpp
class state {
    static constexpr bool value = false;
    static constexpr bool error = true;
};
```

... the selectors would have been the same, though `state` would no longer be
useful in `index()` and `get()`.

`index` is the function used to determine at runtime (or compile time for
`match constexpr`) the current state of the `std::expected` passed to it.

`get` is the function that gets the value out of the `std::expected`, given
the current state as a constant template parameter (`State` above).

Note that the type returned by `index` must be compatible with the first
template parameter of `get` (the same type, implicitly convertible, or
comparable).

Everything in this specialization is required for a type that uses the closed
protocol for `std::alternative_traits`, except `state`; `state` is optional.

## The Open Protocol

Here is an example of a specialization of `std::alternative_traits` for
`std::expected`, using the open protocol:

```cpp
template <>
struct alternative_traits<any> {
  template <class Self, class Pointer>
  struct result {
    explicit operator bool() const noexcept { return pointer != nullptr; }

    decltype(auto) operator*() && noexcept {
      return std::forward_like<Self>(*pointer);
    }

    Pointer pointer;
  };

  static bool has_value(const any& value) noexcept { return value.has_value(); }

  template <class T, class Self>
  static auto try_cast(Self&& self) noexcept {
    using ValueType = remove_reference_t<T>;
    auto* pointer = std::any_cast<ValueType>(std::addressof(self));
    return result<Self, decltype(pointer)>{pointer};
  }
};
```

`has_value` indicates whether the `std::any` is in an empty/null state.  For
another type that is non-nullable, `has_value` should always return `true`.

`try_cast` attempts to cast the subject `std::any` to type `T`.  `Self`
represents the exact type of the subject, including cv and reference
qualifiers; it will always be (possibly cv-ref-qualified `std::any`).  The
value returned by `try_cast` must be explicitly convertible to `bool`, and
must be dereferenceable.  When dereferenced, it must return a reference to the
value of type `T` held within the `std::any`, but with the value category of
the subject (i.e. `Self`).

`result` is the type returned by `try_cast`.  It satisfies the requirements
for the return value of `try_cast` just described.  This is the return type
used by this particular `std::alternative_traits` specialization.  Other
specializations may define different templates with different names, as long as
they satisfy the `try_cast` return value requirements.

Note that dereferencing the value returned by `try_cast` is only done if the
cast is used to initialize a value in the match expression/statement.  Some
matchers only look at the type held within a variant-like subject, but then
don't actually use the value:

```cpp
std::variant<int, std::string> val;

match (val) {
  case { int }         => std::print("That's an int.");   // Type is used in these matchers,
  case { std::string } => std::print("That's a string."); // but not the value of that type.
}
```

`has_value` and `try_cast` are both required for a type that uses the open
protocol for `std::alternative_traits`.

## The Non-Variant-Like Protocol

The open protocol and the closed protocol cover the kinds of variant-like
types that pattern matching will try to unpack.  The process of looking into a
variant-like type and determining the runtime type of the value inside is
called *projection*.  *Projection* only applies to proper variant-like types.

Above we discussed the desire to support enumeration-like, nonintegral types
as well.  The example above was `std::chrono::weekday`.  Clearly, types like
`std::chrono::weekday` are not the same as variant-like types -- you don't
have to `get<>` or `cast` the current value out.  You just compare the value
to one of the list of possible values.

Consider another enumeration-like, nonintegral type, `std::strong_ordering`.

```cpp
template <>
struct alternative_traits<strong_ordering> {
  static constexpr alternative_info alternatives[] = {
      ^^strong_ordering::less,
      ^^strong_ordering::equivalent,
      ^^strong_ordering::greater,
  };
  static constexpr bool has_residual_states = false;

  static constexpr unsigned index(strong_ordering value) noexcept {
    return value.value_ + 1;
  }
};
```

This looks a lot like the closed protocol example, except that it is missing
`get`.  Since we don't actually need to project the value of a
`std::strong_ordering`, we don't need `get`.  However, the other elements are
all required: `alternatives`, `has_residual_states`, and `index`.

# Copious Examples

## Standard Library

These are all the specializations of `std::alternative_traits` in the standard
library, except the ones for `std::expected`, `std::any`, and
`std::strong_ordering`, since they were already shown.  They are all taken
from the pattern matching Clang prototype by Michael, de-uglified for your
viewing pleasure.

The first specialization is for pointers.  This follows the
nullable-types-are-just-variants design philosophy described earlier.

```cpp
template <class T>
struct alternative_traits<T*> {
  static constexpr alternative_info alternatives[] = {
      {meta::reflect_constant(nullptr), /*empty=*/true},
      ^^T,
  };
  static constexpr bool has_residual_states = false;

  enum class state : bool { null = false, value = true };

  // The parameter is templated so nullable library types can reuse this
  // implementation while operating on the actual matching subject.
  template <class Self>
  static constexpr state index(const Self& self) noexcept {
    return self ? state::value : state::null;
  }

  template <state State, class Self>
    requires(State == state::value)
  static constexpr decltype(auto) get(Self&& self) noexcept {
    return match constexpr (is_void_v<T>) -> decltype(auto) {
      case true => ;
      case false => *std::forward<Self>(self);
    };
  }
};
```

This specialization provides a reflection on the two "states" of a pointer:
null and non-null; the non-null case is represented by the value the pointer
points to.  We see this reflected in `alternatives`.  The first element is a
reflection of the `nullptr` constant, and is marked as being the empty state
for `T*`.  The second element is a reflection of the type of the value pointed
to, `T`.

The `Self` template parameter to `index` is pretty useless for a `T *`, but
allows this specialization to be reused for other pointer-like types.  See
`std::alternative_traits<std::{shared,unique}_ptr<T>>` below.

`state` indicates that there are two named selectors for a pointer, `.null`
and `.value`.

`get` is slightly complicated by the fact that, though we can match on a `void
*` pointer, we cannot dereference it.

```cpp
template <class T>
  requires(!is_array_v<T>)
struct alternative_traits<shared_ptr<T>> : alternative_traits<T*> {};

template <class T, class D>
  requires(!is_array_v<T>)
struct alternative_traits<unique_ptr<T, D>> : alternative_traits<T*> {};
```

The standard smart pointers just use the implementation from the pointer
specialization.

```cpp
template <class T>
struct alternative_traits<optional<T>> {
  static constexpr alternative_info alternatives[] = {
      {^^nullopt, /*empty=*/true},
      ^^T,
  };

  static constexpr bool has_residual_states = false;

  enum class state : bool { null = false, value = true };

  static constexpr state index(const optional<T>& value) noexcept {
    return value.has_value() ? state::value : state::null;
  }

  template <state State, class Self>
    requires(State == state::value)
  static constexpr decltype(auto) get(Self&& self) noexcept {
    return *std::forward<Self>(self);
  }
};
```

`std::alternative_traits<std::optional<T>>` is largely the same as
`std::alternative_traits<T*>`.  The implementation details of `index` and
`get` must be different, but `state` and the reflections in `alternatives` are
essentially the same, just replacing `nullptr` with `nullopt`.

```cpp
template <>
struct alternative_traits<chrono::month> {
  static constexpr alternative_info alternatives[] = {
      ^^chrono::January,
      ^^chrono::February,
      ^^chrono::March,
      ^^chrono::April,
      ^^chrono::May,
      ^^chrono::June,
      ^^chrono::July,
      ^^chrono::August,
      ^^chrono::September,
      ^^chrono::October,
      ^^chrono::November,
      ^^chrono::December,
  };
  static constexpr bool has_residual_states = true;

  static constexpr unsigned index(chrono::month value) noexcept {
    return static_cast<unsigned>(value) - 1;
  }
};

template <>
struct alternative_traits<chrono::weekday> {
  static constexpr alternative_info alternatives[] = {
      ^^chrono::Sunday,
      ^^chrono::Monday,
      ^^chrono::Tuesday,
      ^^chrono::Wednesday,
      ^^chrono::Thursday,
      ^^chrono::Friday,
      ^^chrono::Saturday,
  };
  static constexpr bool has_residual_states = true;

  static constexpr unsigned index(chrono::weekday value) noexcept {
    return value.c_encoding();
  }
};

template <>
struct alternative_traits<partial_ordering> {
  static constexpr alternative_info alternatives[] = {
      ^^partial_ordering::less,
      ^^partial_ordering::equivalent,
      ^^partial_ordering::greater,
      ^^partial_ordering::unordered,
  };
  static constexpr bool has_residual_states = false;

  static constexpr unsigned index(partial_ordering value) noexcept {
    return value == partial_ordering::unordered ? 3 : int(value.value_) + 1;
  }
};

template <>
struct alternative_traits<weak_ordering> {
  static constexpr alternative_info alternatives[] = {
      ^^weak_ordering::less,
      ^^weak_ordering::equivalent,
      ^^weak_ordering::greater,
  };
  static constexpr bool has_residual_states = false;

  static constexpr unsigned index(weak_ordering value) noexcept {
    return value.value_ + 1;
  }
};
```

These are all very much alike.  They each implement the Non-Variant-Like
Protocol.

```cpp
template <class... Types>
struct alternative_traits<variant<Types...>> {
  static constexpr alternative_info alternatives[] = { ^^Types... };
  static constexpr bool has_residual_states = true;

  struct state {
    size_t value;

    template <size_t I>
    static constexpr size_t index = I;

    constexpr operator size_t() const noexcept { return value; }
  };

  static constexpr state index(const variant<Types...>& value) noexcept {
    return {value.index()};
  }

  template <size_t I, class _Self>
  static constexpr decltype(auto) get(_Self&& self) noexcept {
    return std::unchecked_get<I>(std::forward<_Self>(self));
  }
};
```

`alternatives` is just the reflection of all the variant's alternatives.

`has_residual_states` is `true` because of `.valueless_by_exception()`.

`state` is doing double duty here, which is why is looks a little confusing.
First, it acts as the result type of `index`; it has an inplicit conversion to
`size_t` so that it is compatible with the first template argument to `get`.
Second, it is acting as a namespace in which the `index` indexed selectors
(`.index<0>`, `.index<1>`, etc.).  This is not strictly necessary.  The author
(one of this paper's authors) was trying to be clever.  Don't be clever like
him.  Just make a separate result type for `index`.

`index` and `get` are very much what you would expect, except that `get` can
use `unchecked_get`, since by the time it gets used, it is already certain
that `I` is the active state.

```cpp
template <>
struct alternative_traits<exception_ptr> {
  static bool has_value(const exception_ptr& p) noexcept { return bool(p); }

  template <class E>
  static const auto* try_cast(const exception_ptr& p) noexcept {
    return std::exception_ptr_cast<remove_cvref_t<E>>(p);
  }
};
```

`std::alternative_traits<std::exception_ptr>` is very similar to the
specialization for `std::any`, since it is also an open type.

## A User-Written Example

Consider `my::Connection`, which is always in one of a fixed set of states.
The states are described by the enumeration `my::ConnectionState`.

```cpp
namespace my {
  enum class ConnectionState { disconnected, connecting, connected };

  class Connection {
  public:
    ConnectionState state() const noexcept;
  };
}
```

For `my::Connection`, we could just define `operator==(my::Connection,
my::ConnectionState)`, and then the `my::ConnectionState` enumerators could be
used in matchers to compare with a `my::Connection` subject.  But then that
`operator==` would be available to *all* our code, which could have other
undesirable effects.  So, to allow matching on the state of a
`my::Connection`, we can specialize `std::alternative_traits`.

```cpp
template <>
struct std::alternative_traits<my::Connection> {
  static constexpr alternative_info alternatives[] = {
    ^^my::ConnectionState::disconnected,
    ^^my::ConnectionState::connecting,
    ^^my::ConnectionState::connected,
  };
  static constexpr bool has_residual_states = false;

  static constexpr my::ConnectionState
  index(const my::Connection& connection) noexcept {
    return connection.state();
  }
};
```

Note that no get operation is provided, because these states are
value-selected and non-projectable.

Now the advertised values can be used directly:

```cpp
void monitor(my::Connection const& connection) {
  match (connection) {
    case my::ConnectionState::disconnected => wait_for_connection();
    case my::ConnectionState::connecting   => show_progress();
    case my::ConnectionState::connected    => begin_work();
  }
}
```

Note that we could also have written `match (connection.state())` to get
access to enum-based matching above.  So, why bother with the specialization
at all?  Calling `.state()` only works at the top level.  If we wanted to look
at a `std::optional<my::Connection>`, there would be no way to indicate that
we want to look at the `.value()` of the object inside the `optional`, rather
than the `my::Connection` object itself:

```cpp
std::optional<my::Connection> connection;

match (connection) {
case std::nullopt => ;                                     // ok
case { my::ConnectionState::disconnected } => reconnect(); // ok with the specialization
                                                           // above, ill-formed without
case _ => ;
}
```
