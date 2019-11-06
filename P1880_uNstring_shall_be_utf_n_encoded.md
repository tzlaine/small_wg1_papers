---
title: "`uNstring` Arguments Shall Be UTF-N Encoded"
document: D1880R1
date: 2019-08-23
audience:
  - SG16
  - LEWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: false

---

# Revisions

  - R1
    - Include `basic_string_view` and `pmr::` strings too.
    - Include a list of affected uses of strings in the current draft.

# `u8string`, `u16string`, and `u32string` Don't Guarantee UTF Endcoding

When SG16 is consulted about which string overloads should be provided for an
interface, we will sometimes recommend providing UTF-encoded overloads.  Those
overloads are specified in terms of C++20's `u8string`, `u16string`, and
`u32string` (though not all of them are usually used in the same interface).

When we give this recommendation, it always comes with the advice, "Those
types don't guarantee encoding, so don't forget to add an _Expects:_ that
indicates that such arguments must be."

Since this is probably universal (no exceptions have come up so far), it seems
like something we should add to [res.on.arguments] in the library
introduction.

# Wording

Append this to the bulleted list in Function arguments [res.on.arguments]:

::: add

- A function argument whose type is (possibly cv-qualified) `basic_string<T>`
  ([string.syn]), `basic_string_view<T>` ([string.view.template]),
  `pmr::basic_string<T>` ([string.syn]), or a `const` reference to one of
  those types, where `T` is `char8_t`, `char16_t`, or `char32_t`, shall be
  respectively UTF-8, UTF-16, or UTF-32 encoded.

:::

# References to Those Three Types in N4835:

# `u8string`-specific: 

- `u8string::[many]`
- `hash<u8string>::operator()`

# `u16string`-specific: 

- `u8string::[many]`
- `hash<u16string>::operator()`

# `u32string`-specific: 

- `u32string::[many]`

# All Three:

- `filesystem::path::path()`
- `bitset::bitset()`
- `basic_format_arg::basic_format_arg()`
- `basic_ostream<charT, traits>& operator<<()`
- `erase()`
- `erase_if()`

- Various overloads of `chrono::from_stream()`, e.g.:

```c++
template<class charT, class traits, class Duration, class Alloc = allocator<charT>>
  basic_istream<charT, traits>&
    from_stream(basic_istream<charT, traits>& is, const charT* fmt,
    sys_time<Duration>& tp,
    basic_string<charT, traits, Alloc>* abbrev = nullptr, minutes* offset = nullptr);
```

- Various overloads of `chrono::parse()`, e.g.:

```c++
template<class charT, class traits, class Alloc, class Parsable>
  unspecified
    parse(const basic_string<charT, traits, Alloc>& format, Parsable& tp);
```

- `locale::operator()`
- `messages::do_open()`
- `quoted()`
- `basic_stringbuf::basic_stringbuf()`
- `basic_stringbuf::str()`
- `basic_istringstream::basic_istringstream()`
- `basic_istringstream::str()`
- `basic_ostringstream::basic_istringstream()`
- `basic_ostringstream::str()`
- `basic_stringstream::basic_istringstream()`
- `basic_stringstream::str()`
- `regex_match()`
- `regex_search()`
- `regex_replace()`
- `basic_regex::basic_regex()`
- `basic_regex::operator=()`
- `basic_regex::assign()`
- `submatch::compare()`
- `match_results::format()`
