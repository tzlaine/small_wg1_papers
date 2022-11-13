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
// {0c, 0c, 3} would work, too.
auto y = submdspan(x, strided_index_range{0zc, 0zc, 3});
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
t[1c] = "some different text";
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
get(t, 1c) = "some different text";
```
:::

Modifications to `std::tuple` and/or `std::get` are out of scope for this
paper, but they show the utility of literals support for
`std::integral_constant`.

```c++
namespace std::literals::integral_constant_literals {
  constexpr int to_int(char c) // exposition only
  {
      int result = 0;

      if (c >= 'A' && c <= 'F') {
        result = static_cast<int>(c) - static_cast<int>('A') + 10;
      } else if (c >= 'a' && c <= 'f') {
        result = static_cast<int>(c) - static_cast<int>('a') + 10;
      } else {
        result = static_cast<int>(c) - static_cast<int>('0');
      } else {
        abort();
      }

      return result;
  }

  template<size_t N>
  constexpr long long parse_ci(const char (&arr)[N]) // exposition only
  {
      long long base = 10;
      size_t offset = 0;

      if (2 < N) {
        bool starts_with_zero = arr[0] == '0';
        bool is_hex = starts_with_zero && arr[1] == 'x';
        bool is_binary = starts_with_zero && arr[1] == 'b';

        if (is_hex) {
          base = 16;
          offset = 2;
        } else if (is_binary) {
          base = 2;
          offset = 2;
        } else if (starts_with_zero) {
          base = 8;
          offset = 1;
        }
      }

      long long number = 0;
      long long multiplier = 1;

      for (size_t i = 0; i < N - offset; ++i) {
        char c = arr[N - 1 - i];
        number += to_int(c) * multiplier;
        multiplier *= base;
      }

      return number;
  }

  template <char ...chars>
  constexpr auto operator"" s()
  {
    return integral_constant<short, parse_ci<sizeof...(chars)>({chars...})>{};
  }
  template <char ...chars>
  constexpr auto operator"" usc()
  {
    return integral_constant<unsigned short, parse_ci<sizeof...(chars)>({chars...})>{};
  }
  template <char ...chars>
  constexpr auto operator"" c()
  {
    return integral_constant<int, parse_ci<sizeof...(chars)>({chars...})>{};
  }
  template <char ...chars>
  constexpr auto operator"" uc()
  {
    return integral_constant<unsigned int, parse_ci<sizeof...(chars)>({chars...})>{};
  }
}
```
