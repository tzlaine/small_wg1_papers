---
title: "Please Don't Rewrite My String Literals"
document: D1879R1
date: 2019-08-23
audience:
  - SG16
  - EWG-I
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: false
monofont: "DejaVu Sans Mono"

---

# The `u8` string literal prefix does not do what you think it does

I was writing tests for a Unicode library for Boost.  The tests included some
non-ASCII string literals, at least one of which included Unicode U+03C2,
"GREEK SMALL LETTER FINAL SIGMA".  It is two UTF-8 code units, `0xcf` and
`0x82`.  In the editor in which I initially wrote that code point, I entered
it as those two code units, and the editor showed as I have here, as a single
glyph representing code point U+03C2.

Note that in every editor I used during the testing process, I saw the source
code as `u8"ς"`.  A hex dump of the source file showed `0xcf 0x82` for this
code point.

I started on Linux, got the tests passing, and then ran them on Mac.  So far,
so good.  Then, I ran them on MSVC, where they did not pass.  Sometime during
the resulting investigation, I wrote this expression, which evaluated to
`true`:

```c++
strlen(u8"ς") == 5
```

After asking around a bit online, I learned about the `/utf-8` MSVC compiler
flag.  That flag fixed my tests.

To see why, consider this variable declaration:

```c++
char str[3] = u8"ς";
```

When compiled with MSVC in a source file with a UTF-8 BOM, or using the
`/utf-8` flag; GCC; or Clang; that declaration is equivalent to this:

```c++
char str[3] = {0xcf, 0x82, 0x0};
```

When compiled with MSVC, without a BOM or `/utf-8` flag, it is equivalent to
this:

```c++
char str[6] = {0xc3, 0x8f, 0xe2, 0x80, 0x9a, 0x0};
```

So, my two UTF-8 code units were silently rewritten to be 5 `char`s in some
encoding that is not UTF-8.  To make matters worse, the declaration without
the `u8` prefix gets me back to the bits I want, regardless of whether the
`utf-8` flags are in use:

```c++
char str[3] = "ς"; // Identical to "char str[3] = {0xcf, 0x82, 0x0};".
```

# That's a lousy user experience

This is well-defined, non-erroneous behavior on the part of all compilers
involved.  All the modes of compilation above are standards conforming as far
as I know.  In [lex.phases], the description of phase 1 of translation starts
with this sentence:

[1]{.pnum}Physical source file characters are mapped, in an
implementation-defined manner, to the basic source character set (introducing
new-line characters for end-of-line indicators) if necessary. The set of
physical source file characters accepted is implementation-defined.

This applies to all characters in the file, regardless of whether they are
within a `u8`-, `u`-, or `U`- prefixed string literal.

Many users who deal in Unicode on Windows (or portably) already know about
this issue and are dealing with it.  However, SG16 is trying their best to get
Unicode support into standard C++.  As such, the hope is that we'll get more
Unicode-naive users to start using Unicode-aware C++ features to future-proof
their code.  Such naive users are going to write nonportable string literals
all over the place if the status quo remains.

Specifically, users must be taught that they cannot use the `u8` string
literal prefix for a string literal that they know to be UTF-8 encoded, or at
least not portably.  They must *omit* the `u8` prefix in some cases to get a
UTF-8 encoded string literal in their final program.

# The fix

To fix this, I want to make it ill-formed for a `u8`-, `u`-, or `U`-prefixed
string literal to appear in a TU whose source and/or execution encoding would
cause the meaning of the literal to change.  The meaning of the literal is
preserved if:

- the bits do not change from what the user wrote in the source file, or

- the literal is transcoded to another UTF format, such that the original code
  points represented by the original bits in the original UTF format are
  preserved.

This lets users specify that they want a particular UTF encoding, likely
seeing it in their editor the way they entered it, and have it appear in their
object code with no unexpected semantics.

Note that this only applies to transformations applied to the contents of
`u8`-, `u`-, or `U`-string literals during phases 1 and 5 of translation.  In
particular, it does not apply to identifiers.  So, this code would retain its
current semantices:

```c++
char ς[] = "status quo";
```

Also note that this change will not silently change any existing code.  This
change also will not cause any existing string literals to be diagnosed, if
they happen to preserve meaning.  In particular, ASCII contents of `u8` string
literals will not be ill-fomed when the source encoding is Windows-1252 or
EBCDIC.

# Semantic changes

Below, I've show differences across different source encodings.  The same
results apply to differences across different execution encodings.

::: tonytable

### Before
```c++
// Source encoding: UTF-8
char str[3] = u8"ς";
assert(strlen(str) == 2); // ok
assert(strlen(str) == 5); // error
```

### After
```c++
// Source encoding: UTF-8
char str[3] = u8"ς";
assert(strlen(str) == 2); // ok
assert(strlen(str) == 5); // error
```

---

```c++
// Source encoding: UTF-16
char str[3] = u8"ς";
assert(strlen(str) == 2); // ok
assert(strlen(str) == 5); // error
```

```c++
// Source encoding: UTF-16
char str[3] = u8"ς";
assert(strlen(str) == 2); // ok
assert(strlen(str) == 5); // error
```

---

```c++
// Source encoding: Windows-1252
char str[3] = u8"ς";
assert(strlen(str) == 2); // error
assert(strlen(str) == 5); // ok
```

```c++
// Source encoding: Windows-1252
char str[3] = u8"ς";      // ill-formed
assert(strlen(str) == 2); // never evaluated
assert(strlen(str) == 5); // never evaluated
```

---

```c++
// Source encoding: Windows-1252
char str[3] = u8"asdf";
assert(strlen(str) == 4); // ok
```

```c++
// Source encoding: Windows-1252
char str[3] = u8"asdf";   // still well-formed
assert(strlen(str) == 4); // ok
```

---

```c++
// Any source encoding
char ς[3] = u8"asdf";     // well-formed, weird
assert(strlen(str) == 4); // ok
```

```c++
// Any source encoding
char ς[3] = u8"asdf";     // still well-formed
assert(strlen(str) == 4); // ok
```

:::
