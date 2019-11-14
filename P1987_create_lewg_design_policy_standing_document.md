---
title: "Create LEWG Design Policy Standing Document"
document: D1987R0
date: 2019-11-12
audience:
  - LEWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: false

---

# Background

At the 2019 Cologne meeting, LEWG voted to accept several of the LEWG policies
proposed in [@P1655R0], and to create a standing document including the
accepted policies.  If approved, this paper will become that standing
document.

# Proposal

## Use of `explicit` Constructors

A constructor callable with a single argument or a conversion operator should
be declared `explicit` unless it:

- is a conversion between two types that are essentially the same;
- preserves all data during conversion;
- imposes little or no performance penalty;
- does not throw; and
- results in a memory-safe value or is designed to be a reference/view type.

## Naming of Predicate-Like Type Traits

Predicate-like type traits should be prefixed with `is_` or `has_`, as
appropriate.

## Naming of "Erased Types"

An "erased type" is a type whose main purpose is to be used
runtime-polymorphically without using inheritence.  Examples are `any` and
`function`.

An erased type should be named `any_C`, where `C` is the name of the concept
that it represents.

## Interfaces With Optional Timeouts

For a standard library function set or overload set that represents an
operation with an optional timeout, the optional timeout should not be
represented with a defaulted function parameter.  Instead, overloads for
operation `X` should be provided, like this:

```c++
X(...)
try_X(...) -> bool
try_X_for(..., chrono::duration) -> bool
try_X_until(..., chrono::time_point) -> bool
```

In particular, an absolute timeout is represented by `chrono::time_point` and
a relative timeout is represented by `chrono::duration`.
