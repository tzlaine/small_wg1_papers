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
    - Include `basic_string_view` too.

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
  ([basic.string]) or `basic_string_view<T>` ([string.view.template]), or a
  reference to one of those types, where `T` is `char8_t`, `char16_t`, or
  `char32_t`, shall be respectively UTF-8, UTF-16, or UTF-32 encoded.

:::
