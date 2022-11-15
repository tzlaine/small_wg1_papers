---
title: "std::integral_constant literals"
document: D0000R0
date: 2022-11-13
audience:
  - EWG-I
  - EWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: false
monofont: "DejaVu Sans Mono"

---

# The ergonomics of `std::integral_constant<int>`

`std::integral_constant<int>` is used in lots of places to communicate a
constant integral value to a given interface.  The length of its spelling
makes it very verbose.  Fortunately, we can do a lot better.

::: tonytable

### Before
```c++
// From P2630R1
auto y = submdspan(x, strided_index_range{integral_constant<size_t, 0>{}, integral_constant<size_t, 10>{}, 3});
```

### After
```c++
using namespace std::literals::integal_constant_literals;
// {0ic, 0ic, 3} would work, too.
auto y = submdspan(x, strided_index_range{0zic, 0zic, 3});
```

:::

# Other opportunities that these literals may open up

If we had easily spelled integral constants, we could add an interface to get
values out of tuples using a much more natural syntax.

If we added an index operator to `std::tuple` that takes a
`std::integral_constant<size_t>`:

::: tonytable

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
:::

Or, if we added an overload of `std::get()` that takes a
`std::integral_constant<size_t>`:

::: tonytable

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
:::

Modifications to `std::tuple` and/or `std::get` are out of scope for this
paper, but they show the utility of literals support for
`std::integral_constant`.

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
