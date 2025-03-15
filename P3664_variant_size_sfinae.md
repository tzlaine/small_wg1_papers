---
title: "Make `variant_size` SFINAE friendly"
document: P3664R0
date: today
audience:
  - LEWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: true
monofont: "DejaVu Sans Mono"

---

# Relationship to previous work

When `tuple_size` was introduced, it was not SFINAE friendly.  [@LWG2770]
notes that this caused a problem with structured bindings.  This is taken
directly from the issue:

> Consider:
>
> ```c++
> #include <utility>
>
> struct X { int a, b; };
> const auto [x, y] = X();
> ```
>
> This is ill-formed: it triggers the instantiation of `std::tuple_size<const X>`,
> which hits a hard error because it tries to use `tuple_size<X>::value`, which
> does not exist. The code compiles if `<utility>` (or another header providing
> tuple_size) is not included.
>
> It seems that we either need a different strategy for decomposition
> declarations, or we need to change the library to make the tuple_size partial
> specializations for cv-qualifiers SFINAE-compatible.
>
> The latter seems like the better option to me, and like a good idea regardless
> of decomposition declarations.

# Design

This paper wants to make the analogous tweak to `variant_size` as was used in
the accepted resolution of [@LWG2770].  Implementation experience with it has
shown the same non-SFINAE limitation is causing similar problems in the
implementation.

# Wording

In [variant.helper]{.sref}:

[2]{.pnum} Let `VS` denote `variant_size<T>` of the cv-unqualified type `T`. [Then]{.rm}[If the expression `VS::value` is well-formed when treated as an unevaluated operand, then]{.add} each specialization of the template meets the *Cpp17UnaryTypeTrait* requirements (`[meta.rqmts]`) with a base characteristic of `integral_constant<size_t, VS::value>`.
[Otherwise, it has no member `value`.]{.add}

::: add

Access checking is performed as if in a context unrelated to `VS` and `T`. Only the validity of the immediate context of the expression is considered.

::: note
The compilation of the expression can result in side effects such as the instantiation of class template specializations and function template specializations, the generation of implicitly-defined functions, and so on. Such side effects are not in the “immediate context” and can result in the program being ill-formed.
:::

:::

In [version.syn]{.sref}:

`#define __cpp_lib_variant`{.cpp}                           [`202306`{.cpp}]{.rm}[??????]{.add}`L // also in <variant>`{.cpp}