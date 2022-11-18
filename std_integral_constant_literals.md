---
title: "`std::integral_constant` literals"
document: D0000R0
date: 2022-11-13
audience:
  - LEWG-I
  - LEWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: false
monofont: "DejaVu Sans Mono"

---

# The ergonomics of `std::integral_constant<int>` can be improved

`std::integral_constant<int>` is used in lots of places to communicate a
constant integral value to a given interface.  The length of its spelling
makes it very verbose.  Fortunately, we can do a lot better.

::: tonytable

---

### Before
```c++
// From P2630R1
auto y = submdspan(x, strided_index_range{integral_constant<size_t, 0>{}, integral_constant<size_t, 10>{}, 3});
```

### After
```c++
using namespace std::literals::integal_constant_literals;
// strided_index_range{0ic, 10ic, 3} would work, too.
auto y = submdspan(x, strided_index_range{0uzic, 10uzic, 3});
```

---

:::

The use of `std::integral_constant<size_t>` above is fairly limited; there are
only two specializations present.  However, more complicated expressions
containing more `std::integral_constant<size_t>`s benefit even more from the
terseness of literals:

::: tonytable

---

### Before
```c++
auto y = submdspan(x, compute_index_range(integral_constant<size_t, 0>{},
                                          integral_constant<size_t, 3>{},
                                          integral_constant<size_t, 8>{},
                                          integral_constant<size_t, 5>{},
                                          integral_constant<size_t, 2>{},
                                          integral_constant<size_t, 10>{});
```

### After
```c++
using namespace std::literals::integal_constant_literals;
auto y = submdspan(x, compute_index_range(0ic, 3ic, 8ic, 5ic, 2ic, 10ic));
```

---

:::

The absence of syntatcic noise when using literals is dramatic in this case.
With the advent of `std::mdspan` and `std::submdspan()`, longer expressions
involving more `std::integral_constant`s are more likely to occur for more
users.

# These literals may open up other code-improvement opportunities

If we had easily spelled integral constants, we could add an interface to get
values out of tuples using a much more natural syntax.

For instance, if we added an index operator to `std::tuple` that takes a
`std::integral_constant<size_t>`:

::: tonytable

---

### Before
```c++
auto t = tuple<int, string>(0, "some text");
get<1>(t) = "some different text";
```

### After
```c++
using namespace std::literals::integal_constant_literals;
auto t = tuple<int, string>(0, "some text");
t[1ic] = "some different text";
```

---

:::

Or, if we added an overload of `std::get()` that takes a
`std::integral_constant<size_t>`:

::: tonytable

---

### Before
```c++
auto t = tuple<int, string>(0, "some text");
get<1>(t) = "some different text";
```

### After
```c++
using namespace std::literals::integal_constant_literals;
auto t = tuple<int, string>(0, "some text");
get(t, 1ic) = "some different text";
```

---

:::

Modifications to `std::tuple` and/or `std::get` are out of scope for this
paper, but they show the utility of `std::integral_constant` literals.

# Proposed design

Add a UDL for each permutation of the literal suffixes in
[lex.icon]/integer-literal.  Each UDL will return a `std::integral_constant<T, V>`,
where `T` is the type that corresponds to the literal suffixes, and `V` is the
value specified by the characters given to the UDL.

Each UDL mandates that the number parsed is `<=
std::numberic_limits<T>::max()`.  This matches user expectations when writing
integral literals with out-of-bounds values, except that it turns UB into
ill-formed code.  This is almost certainly an improvement, since it happens at
compile time.

Since a UDL will never include a sign character, in order to be able to write
natural-looking literals like `-42ic`, also add a unary `operator-` for
`std::integral_constant`.

## A minor limitation

You cannot spell the minimal value for signed integral types using the
proposed literals, because we are not generating negative constants by parsing
integers (we never see the minus sign, remember).  Instead, we are parsing
unsigned integers and then using unary negation.  Assuming `INT_MIN ==
-2147483648` and `INT_MAX == 2147483647`:

```c++
-2147483648ic; // Ill-formed!
```

Before the negation can occur, the implementation must reject `2147483648ic`,
because it is greater thatn `INT_MAX`.  There is simply no way to spell
`std::integral_constant<T, std::numberic_limits<T>::min()>` as a literal, if
`T` is signed.  Fortunately, one can still spell it the verbose way.

# Implementation experience

An `integral_constant` with the proposed semantics has been a part of
[Boost.Hana](https://www.boost.org/doc/libs/1_80_0/libs/hana/doc/html/index.html)
since its initial release in May of 2016.  Its literals have been used by
many, many users.

I independently implemented similar support, including additional checks that
the parsed number can fit within the range of the integral type being
generated.  It was straightforward; it only took me about 4 hours, including
tests.  The implementation can be found on
[Github](https://github.com/tzlaine/ic_literals).

# Wording

In [meta.help]{.sref}:

```cpp
namespace std {
  template<class T, T v> struct integral_constant {
    static constexpr T value = v;

    using value_type = T;
    using type = integral_constant<T, v>;

    constexpr operator value_type() const noexcept { return value; }
    constexpr value_type operator()() const noexcept { return value; }

    @[`constexpr integral_constant<T, -V> operator-();`]{.add}@
  };
}
```














TODO: Add a Mandates: to operator-() that -V is in the range of T.

```c++
namespace std {
  template<typename T, T V>
  constexpr integral_constant<T, -V> operator-(integral_constant<T, V>)
  {
    return {};
  }
}

namespace std::literals::integral_constant_literals {
  template<typename TargetType, size_t N>
  constexpr TargetType parse_ci(const char (&arr)[N]) // exposition only
  {
      int base = 10;
      int offset = 0;

      if (arr[0] == '0') {
        if (2 < N) {
          const bool is_hex = arr[1] == 'x' || arr[1] == 'X';
          const bool is_binary = arr[1] == 'b';
  
          if (is_hex) {
            base = 16;
            offset = 2;
          } else if (is_binary) {
            base = 2;
            offset = 2;
          } else {
            base = 8;
            offset = 1;
          }
        } else if (N == 2) {
          base = 8;
          offset = 1;
        }
      }

      const auto f = std::begin(arr) + offset, l = std::end(arr);
      TargetType x{};
      const auto result = std::from_chars(f, l, x, base);
      if (result.ptr != l || result.ec != errc{}) {
        throw logic_error("");
      }
      return x;
  }

  // (signed) int
  template <char ...chars>
  constexpr auto operator"" c()
  {
    constexpr auto x = parse_ci<int, chars...>();
    return integral_constant<remove_const_t<decltype(x)>, x>{};
  }

}
```
