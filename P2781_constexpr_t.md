---
title: "`std::integral_constant` Literals and `std::constexpr_t`"
document: D2781R0
date: 2023-01-28
audience:
  - LEWG-I
  - LEWG
author:
  - name: Matthias Kretz
    email: <m.kretz@gsi.de>
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: true
monofont: "DejaVu Sans Mono"

---

# Relationship to previous work

This paper is co-authored by the authors of P2725R1
("`std::integral_constant` Literals") and P2772R0
("`std::integral_constant` literals do not suffice — `constexpr_t`?").  This
paper supersedes both of those previous papers.

# The ergonomics of `std::integral_constant<int>` can be improved

`std::integral_constant<int>` is used in lots of places to communicate a
constant integral value to a given interface.  The length of its spelling
makes it very verbose.  Fortunately, we can do a lot better.

::: tonytable

### Before
```c++
// From P2630R1
auto const sir =
  strided_index_range{integral_constant<size_t, 0>{},
                      integral_constant<size_t, 10>{},
                      3};
auto y = submdspan(x, sir);
```

### After
```c++
using namespace std::literals::integal_constant_literals;
// strided_index_range{0ic, 10ic, 3} would work, too.
auto y = submdspan(x, strided_index_range{0uzic, 10uzic, 3});
```

:::

The use of `std::integral_constant<size_t>` above is fairly limited; there are
only two specializations present.  However, more complicated expressions
containing more `std::integral_constant<size_t>`s benefit even more from the
terseness of literals:

::: tonytable

### Before
```c++
auto const sir =
  compute_index_range(integral_constant<size_t, 0>{},
                      integral_constant<size_t, 3>{},
                      integral_constant<size_t, 8>{},
                      integral_constant<size_t, 5>{},
                      integral_constant<size_t, 2>{},
                      integral_constant<size_t, 10>{});
auto y = submdspan(x, sir);
```

### After
```c++
using namespace std::literals::integal_constant_literals;
auto const sir =
  compute_index_range(0ic, 3ic, 8ic, 5ic, 2ic, 10ic);
auto y = submdspan(x, sir);
```

:::

The absence of syntactic noise when using literals is dramatic in this case.
With the advent of `std::mdspan` and `std::submdspan()`, longer expressions
involving more `std::integral_constant`s are more likely to occur for more
users.

## `std::integral_constant` literals are by themselves not enough

Parameters passed to a `constexpr` function lose their `constexpr`-ness.
Introducing literals for `std::integral_constant` only improves the use of
that template in interfaces; what about all the other `constexpr`-friendly C++
types?

Consider:

```c++
template<typename T>
struct my_complex
{
    T re, im;
};

inline constexpr short foo = 2;

template<typename T>
struct X
{
    void f(auto c)
    {
        // c is to be used as a constexpr value here
    }
};
```

We would like to be able to call `X::f()` with a value, and have that value
keep its `constexpr`-ness, even if we don't pass a `std::integral_constant`.
Let's introduce a template "`constexpr_t`" that holds a `constexpr` value that
it is given as an non-type template parameter.

```c++
namespace std {
  template<class T, T X>
    requires same_as<remove_cvref_t<T>, T>
  struct constexpr_t
  {
    static constexpr T value = X;

    using value_type = T;
    using type = constexpr_t;

    constexpr operator value_type() const { return X; }

    // The rest of the members are discussed below ....
  };
}
```

Now we can write this.

```c++
template<typename T>
void g(X<T> x)
{
    x.f(std::constexpr_t<1>{});
    x.f(std::constexpr_t<2uz>{});
    x.f(std::constexpr_t<3.0>{});
    x.f(std::constexpr_t<4.f>{});
    x.f(std::constexpr_t<foo>{});
    x.f(std::constexpr_t<my_complex(1.f, 1.f)>{});
}
```

Let's now add a `constexpr` variable template with a shorter name, say `c`.

```c++
namespace std {
  template<auto X>
  inline constexpr constexpr_t<remove_const_t<decltype(X)>, X> c{};
}
```

And now we can write this.

```c++
template<typename T>
void g(X<T> x)
{
    x.f(std::c<1>);
    x.f(std::c<2uz>);
    x.f(std::c<3.0>);
    x.f(std::c<4.f>);
    x.f(std::c<foo>);
    x.f(std::c<my_complex(1.f, 1.f)>);
}
```

`std::constexpr_t` and `std::c` solve all the same problems as are solved by
`std::integral_constant` literals (with slightly different spelling), plus
more -- the last four calls to `X::f()` don't work with the literals alone.

## The difference in template parameters to `std::constexpr_t` and `std::c`

`std::c` takes an `auto` NTTP.  `std::constexpr_t` takes a type `T` and an
NTTP `X` for type `T`.  Why is this?

The rationale is that when a user writes `std::c<foo>`, we want the notation
to be as terse as possible.  Moreover, the user is in complete control of the
`X` value that she puts in the brackets.  In a generic context, that might not
always be the case; you sometimes might want to force the type held by your
`std::constexpr_t` to be a particular type, say `size_t`.  For instance, say
you have an existing function `foo()`:

```c++
template<size_t X>
auto foo(std::constexpr_t<size_t, X> x) {
    return /* ... */;
}
```

And now let's say you want to write a function `bar()` such that `bar<X>()`
calls `foo(std::constexpr<size_t, X>{})`:

```c++
template<auto X>
void bar() {
    std::constexpr_t<size_t, X> x;
    auto y = bar(x);
    // Use y ....
}
```

If you didn't care what type the `T` parameter to `std::constexpr_t` was, you
could use `std::c<X>` instead.  Having both notational options is useful.

## Options for writing `std::integral_constant` literals

### Option 1: Provide `std::integral_constant` literals

This is the approach from P2725R1 ("`std::integral_constant` Literals").
It has the advantage of being the most terse.

In order for these literals to work, `std::integral_constant` must also get a
unary `operator-()` added to it, so that literals like `-5ic` produce a
`std::integral_constant` rather than an `int`.  The addition of this operator
may break existing code, albeit in rare circumstances.  For instance:

```c++
void f(std::same_as<int> auto);

void g(auto x) {
    f(-x); // valid now, ill-formed with addition of std::integral_constant::operator-()
}

void h(){
    g(std::integral_constant<int, 1>());
}
```

### Option 2: Provide `std::constexpr_t` literals compatible with `std::integral_constant`

This option creates UDLs with the same names as proposed in from P2725R1
("`std::integral_constant` Literals").  However, the UDLs return
`std::constexpr_t`s instead of `std::integral_constant`s.  It also has the
advantage of being the most terse.

There is no need to add unary `operator-()` to `std::integral_constant` in
order for these literals to work, since they will use unary
`std::constexpr_t::operator-()` instead (see below).

Since there are few (if any) interfaces in `std` that accept
`std::integral_constant`s currently, and since those being proposed are not
yet merged into a draft of the IS, we probably don't lose anything by making
these UDLs return `std::constexpr_t` instead of `std::integral_constant`.

We also side-step the potential for code breakage outlined in Option 1.

### Option 3: Provide a `constexpr` variable template for `std::integral_constant`s

Using the same technique as for `std::constexpr_t` and `std::c`, we could
introduce a `constexpr` variable template `std::ic` for writing
`std::integral_constant`s.

```c++
namespace std {
  template<auto X>
  inline constexpr integral_constant<remove_const_t<decltype(X)>, X> ic{};
}
```

### Option 3 is orthogonal to the other two

While Options 1 and 2 are mutually exclusive, the decision whether to adopt
Option 3 is independent.

Returning to "After" case from the second example at the top of the paper, you
can see how the literals are used.

```c++
using namespace std::literals;
auto const sir =
  compute_index_range(0ic, 3ic, 8ic, 5ic, 2ic, 10ic);
auto y = submdspan(x, sir);
```

That's pretty good, and using `std::ic` instead gives this, which is clearly
not as good.

```c++
auto const sir =
  compute_index_range(std::ic<0>, std::ic<3>, std::ic<8>, std::ic<5>, std::ic<2>, std::ic<10>);
auto y = submdspan(x, sir);
```

However, in the `std::ic` case, you get to leave off the `using` declaration.
Having both options allows the user to get a reasonably terse syntax in cases
where they cannot add a `using` declaration to the current scope, and an even
terser syntax when they can.

The authors both favor Option 2 alone.

# Making `constexpr_t` more useful

`constexpr_t` is essentially a wrapper.  It takes a value `X` of some
structural type `T`, and represents `X` in such a way that we can continue to
use `X` as a compile-time constant, regardless of context.  As such,
`constexpr_t` should be implicitly convertible to `T`; this is already
reflected in the design presented above.  For the same reason, `constexpr_t`
should provide all the operations that the underlying type has.  Though we
cannot predict what named members the underlying type `T` has, we *can* guess
at all the operator overloads it might have.

So, by adding conditionally-defined overloads for all the overloadable
operators, we can make `constexpr_t` as natural to use as many of the types it
might wrap.

```c++
namespace std {
  template<class T, T X>
    requires same_as<remove_cvref_t<T>, T>
  struct constexpr_t
  {
    static constexpr T value = X;

    using value_type = T;
    using type = constexpr_t;

    constexpr operator value_type() const { return X; }

    // unary -
    constexpr auto operator-() const
      requires requires { constexpr_t<decltype(-X), -X>{}; }
        { return constexpr_t<decltype(-X), -X>{}; }

    // binary + and -
    template<class U>
      friend constexpr constexpr_t<decltype(X + U::value), (X + U::value)>
        operator+(constexpr_t, U) { return {}; }
    template<class U>
      requires not_constexpr_t<U>
        friend constexpr constexpr_t<decltype(U::value + X), (U::value + X)>
          operator+(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X - U::value), (X - U::value)>
        operator-(constexpr_t, U) { return {}; }
    template<class U>
      requires not_constexpr_t<U>
        friend constexpr constexpr_t<decltype(U::value - X), (U::value - X)>
          operator-(U, constexpr_t) { return {}; }

    // etc... (full listing later)
  };
}
```

These operators are defined in such a way that they behave just like the
operations on underlying the `T` and `U` values would, including promotions and
coercions.  For example:

```c++
static_assert(std::is_same_v<
              decltype(std::c<42> - std::c<13u>),
              std::constexpr_t<unsigned int, 29u>>);
```

Each operation is only defined if the underlying operation on `X` and `Y` is
defined.  Each operation additionally requires that the result of the
underlying operation have a structural type.

The mutating operations are left out, because none of them makes sense -- the
type of the mutated value would have to change, since the value is itself part
of the type.

All the remaining operations are included, even the index and call operators.
The rationale for this is that a user may want to make some sort of
compile-time domain-specific embedded language using operator overloading, and
having all but a couple of the operators specified would frustrate that
effort.

The only downside to adding `std::constexpr_t::operator()()` is that it would
represent a break from the design of `std::integral_constant`, making it an
imperfect drop-in replacement for that template.

The operators are designed to interoperate with other types and templates that
have a constexpr static `value` member.  This works with `std::constexpr_t`s
of course, but also `std::integral_constant`s, and user-provided types as
well.  For example:

```c++
struct my_type { constexpr static int value = 42; };

void foo()
{
    constexpr auto zero = my_type{} - std::c<42>;  // Ok.
    // ...
}
```

Note that the addition of these operators is in line with the poll:

"Add a new robust integral constant type with all the numerical operators, as
proposed in P2772R0, and use that for these literals instead of
`std::integral_constant`"?

+----+---+---+---+----+
| SF | F | N | A | SA |
+====+===+===+===+====+
| 4  |7  |1  |1  | 1  |
+----+---+---+---+----+

... taken in the 2023-01-17 Library Evolution telecon.

Note that the one SA said he would not be opposed if the word "integral" was
stricken from the poll, and the design of `std::constexpr_t` is not limited to
integral types.

# What about strings?

As pointed out on the reflector, `std::c<"foo">` does not work, because of
language rules.  However, it's pretty easy for users to add an NTTP-friendly
string wrapper type, and then use that with `std::c<>`.

```c++
template<size_t N>
struct strlit
{
    constexpr strlit(char const (&str)[N]) { std::copy_n(str, N, value); }
    char value[N];

    friend std::ostream & operator<<(std::ostream & os, strlit l)
    {
        assert(!l.value[N - 1] && "value must be null-terminated");
        return os.write(l.value, N - 1);
    }
};

int main()
{
    auto f = std::c<strlit("foo")>;
    std::cout << f; // Prints "foo".
}
```

# Design

## Add literals for `std::integral_constant` (Option 1) or `std::constexpr_t` (Option 2)

Add a UDL for each permutation `P` of the literal suffixes in
[lex.icon]/integer-literal, with the suffix "`ic`" appended to `P`
(e.g. "`ull`" + "`ic`" = "`ullic`").  Each UDL will return a
`std::integral_constant<T, V>`/`std::constexpr_t<T, V>`, where `T` is the type
that corresponds to `P`, and `V` is the integral value specified by the
characters given to the UDL.

Note that the simpler suffix "`c`" (for "`const`{.cpp}") is not usable,
because `c` is a valid hex digit.  This makes us sad.

Each call to one of the proposed UDLs is ill-formed when the number parsed is
`> std::numberic_limits<T>::max()`.  This matches user expectations when
writing integral literals with out-of-bounds values, except that it turns UB
into ill-formed code.  This is almost certainly an improvement, since it
happens at compile time (UB is highly problematic at compile time).

If choosing Option 1, we also need to add a unary
`std::integral_constant::operator-`, because a UDL will never include a sign
character.  Without this, you could not write natural-looking literals like
`-42ic`.

## Add `constexpr_t`

```c++
namespace std {
  template<class T, T X>
    requires same_as<remove_cvref_t<T>, T>
      struct constexpr_t;

  template<class T>
    constexpr bool @*not-constexpr-t*@ = true;                      // @*exposition only*@
  template<class T, T X>
    constexpr bool @*not-constexpr-t*@<constexpr_t<T, X>> = false;  // @*exposition only*@

  template<class T, T X>
    requires same_as<remove_cvref_t<T>, T>
  struct constexpr_t
  {
    static constexpr T value = X;

    using value_type = T;
    using type = constexpr_t;

    constexpr operator value_type() const { return X; }

    constexpr auto operator+() const
      requires requires { constexpr_t<decltype(+X), +X>{}; }
        { return constexpr_t<decltype(+X), +X>{}; }
    constexpr auto operator-() const
      requires requires { constexpr_t<decltype(-X), -X>{}; }
        { return constexpr_t<decltype(-X), -X>{}; }
    constexpr auto operator~() const
      requires requires { constexpr_t<decltype(~X), ~X>{}; }
        { return constexpr_t<decltype(~X), ~X>{}; }
    constexpr auto operator!() const
      requires requires { constexpr_t<decltype(!X), !X>{}; }
        { return constexpr_t<decltype(!X), !X>{}; }
    constexpr auto operator&() const
      requires requires { constexpr_t<decltype(&X), &X>{}; }
        { return constexpr_t<decltype(&X), &X>{}; }
    constexpr auto operator*() const
      requires requires { constexpr_t<decltype(*X), *X>{}; }
        { return constexpr_t<decltype(*X), *X>{}; }

    template<class U>
      friend constexpr constexpr_t<decltype(X << U::value), (X << U::value)>
        operator<<(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value << X), (U::value << X)>
          operator<<(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X >> U::value), (X >> U::value)>
        operator>>(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value >> X), (U::value >> X)>
          operator>>(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X * U::value), (X * U::value)>
        operator*(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value * X), (U::value * X)>
          operator*(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X / U::value), (X / U::value)>
        operator/(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value / X), (U::value / X)>
          operator/(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X % U::value), (X % U::value)>
        operator%(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value % X), (U::value % X)>
          operator%(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X + U::value), (X + U::value)>
        operator+(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value + X), (U::value + X)>
          operator+(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X - U::value), (X - U::value)>
        operator-(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value - X), (U::value - X)>
          operator-(U, constexpr_t) { return {}; }

    template<class U>
      friend constexpr constexpr_t<decltype(X < U::value), (X < U::value)>
        operator<(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value < X), (U::value < X)>
          operator<(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X > U::value), (X > U::value)>
        operator>(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value > X), (U::value > X)>
          operator>(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X <= U::value), (X <= U::value)>
        operator<=(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value <= X), (U::value <= X)>
          operator<=(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X >= U::value), (X >= U::value)>
        operator>=(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value >= X), (U::value >= X)>
          operator>=(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X == U::value), (X == U::value)>
        operator==(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value == X), (U::value == X)>
          operator==(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X != U::value), (X != U::value)>
        operator!=(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value != X), (U::value != X)>
          operator!=(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X <=> U::value), (X <=> U::value)>
        operator<=>(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value <=> X), (U::value <=> X)>
          operator<=>(U, constexpr_t) { return {}; }

    template<class U>
      friend constexpr constexpr_t<decltype(X && U::value), (X && U::value)>
        operator&&(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value && X), (U::value && X)>
          operator&&(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X || U::value), (X || U::value)>
        operator||(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value || X), (U::value || X)>
          operator||(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X & U::value), (X & U::value)>
        operator&(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value & X), (U::value & X)>
          operator&(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X | U::value), (X | U::value)>
        operator|(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value | X), (U::value | X)>
          operator|(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X ^ U::value), (X ^ U::value)>
        operator^(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value ^ X), (U::value ^ X)>
          operator^(U, constexpr_t) { return {}; }

    template<class U>
      friend constexpr constexpr_t<decltype(X , U::value), (X , U::value)>
        operator,(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value , X), (U::value , X)>
          operator,(U, constexpr_t) { return {}; }
    template<class U>
      friend constexpr constexpr_t<decltype(X ->* U::value), (X ->* U::value)>
        operator->*(constexpr_t, U) { return {}; }
    template<class U>
      requires @*not-constexpr-t*@<U>
        friend constexpr constexpr_t<decltype(U::value ->* X), (U::value ->* X)>
          operator->*(U, constexpr_t) { return {}; }

    template<class... Args>
      constexpr auto operator()(Args... args) const
        -> constexpr_t<decltype(X(Args{}...)), X(Args{}...)>
          { return {}; }
    template<class... Args>
      constexpr auto operator()(Args... args) const
        -> constexpr_t<decltype(X[Args{}...]), X[Args{}...]>
          { return {}; }
  };

  template<auto X>
  inline constexpr constexpr_t<remove_const_t<decltype(X)>, X> c{};
}
```

## Add a feature macro

Add a new feature macro, `__cpp_lib_constexpr_t`.

# Implementation experience

Look up a few lines to see an implementation of `std::constexpr_t`.  At the
time of this writing, there is one caveat: `operator[]()` looks correct to the
authors, but does not work in any compiler tested, due to the very limited
multi-variate `operator[]` support in even the latest compilers.

Additionally, an `integral_constant` with most of the operator overloads has
been a part of
[Boost.Hana](https://www.boost.org/doc/libs/1_80_0/libs/hana/doc/html/index.html)
since its initial release in May of 2016.  Its literals have been used by
many, many users.

Using the Hana implementation as a guide, one of the authors independently
implemented the UDLs proposed in this paper, including checks (missing from
the Hana implementation) that the parsed number can fit within the range of
the integral type associated with the UDL.  It was straightforward; it took
about 4 hours, including tests.  The implementation can be found on
[Github](https://github.com/tzlaine/ic_literals).  The C++26 implementation is
nearly trivial, since `std::from_chars()` is `constexpr`{.cpp} in C++23 and
later.

# Possible polls for LEWG

- We want `std::constexpr_t` and `std::c` in some form.

- We should call `std::c`:
  - `std::c`
  - `std::const_`
  - `std::Const`
  - `std::cnst`

- We want the `std::constexpr_t` operator overloads.

- We want to add UDL support to `std::integral_constant`.

- We want to add UDL support to `std::constexpr_t`. Note that a "yes" to this
  poll is mutually exclusive with a "yes" to the previous poll.

- We want `std::ic<>` to ease writing `std::integral_constant`s.  (This answer
  is almost certainly "no" if we add the UDLs for `std::integral_constant`.)

- We want all the operator overloads to `std::integral_constant` as well.
  (We'd have to either leave out `std::integral_constant::operator()()`
  entirely, or constrain it not to be defined in the nullary case, since
  `std::integral_constant` already has an `operator()()`.)
