---
title: "Unicode in the Library, Part 2: Normalization"
document: P2729R0
date: 2022-11-20
audience:
  - SG-16 Unicode
  - LEWG-I
  - LEWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: true
monofont: "DejaVu Sans Mono"

---

# Motivation

I’m proposing normalization interfaces that meet certain design requirements
that I think are important; I hope you’ll agree:

- Ranges are the future. We should have range-friendly ways of doing
  transcoding. This includes support for sentinels and lazy views.

- A null-terminated string should not be treated as a special case. The
  ubiquity of such strings means that they should be treated as first-class
  strings.

- Input may come from UTF-8, UTF-16, or UTF-32 strings (though UTF-32 is
  extremely uncommon in practice).  There should be a single overload of each
  normalization interface, so that the user does not need to change code when
  the input is changed from UTF-N to UTF-M.

- The Unicode algorithms are low-level tools that most C++ users will not need
  to touch, even if their code needs to be Unicode-aware.  C++ users should
  also be provided higher-level, string-like abstractions (provisionally
  called `std::text`) that will handle all the messy Unicode details, leaving
  C++ users to think about their program instead of Unicode).

# The shortest Unicode normalization primer I can manage

You can have different strings of code points that mean the same thing.  For
example, you could have the code point "ä" (U+00E4 Latin Small Letter A with
Diaeresis), or you could have the two code points "a" (U+0061 Latin Small
Letter A) and "¨̈" (U+0308 Combining Diaeresis).  The former represents "ä" as
a single code point, the latter as two.  Unicode rules state that both strings
must be treated as identical.

To make such comparisons more efficient, Unicode has normalization forms.  If
all the text you ever compare is in the same normalization form, they won't be
in different forms like "ä" vs. "a¨" -- they'll all be represented the same
way, and will therefore compare bitwise-equal to each other if they represent
the same text.

There are four official normalization forms.  The first two are NFC
("Normalization Form: Composed"), and NFD ("Normalization Form: Decomposed").
There are these two other forms NFKC and NFKD that you can safely ignore; they
are seldom-used variants of NFC and NFD, respectively.

NFC is the most compact of these four forms.  It is near-ubiquitous on the
web, as W3C recommends that web sites use it exclusively.

There's this other form FCC, too.  It's really close to NFC, except that it is
not as compact in some corner cases (though it is identical to NFC in most cases).
It's really handy when doing something called collation, which is not yet
proposed.  It's coming, though.

# The stream-safe format

Unicode text often contains sequences in which a noncombining code point
(e.g. "A") is followed by one or more combining code points (e.g. some number
of umlauts). It is valid to have an "A" followed by 100 million umlauts. This
is valid but not useful. Unicode specifies something called the Stream-Safe
Format. This format inserts extra code points between combiners to ensure that
there are never more than 30 combiners in a row. In practice, you should never
need anywhere near 30 to represent meaningful text.

Long sequences of combining characters create a problem for algorithms like
normalization and grapheme breaking; the grapheme breaking algorithm may be
required to look ahead a very long way in order to determine how to handle the
current code point. To address this, Unicode allows a conforming
implementation to assume that a sequence of code points contains graphemes of
at most 31 code points. This is known as the Stream-Safe Format assumption.
All the proposed interfaces here and in the papers to come make this
assumption.

The stream-safe format is very important.  Its use prevents the Unicode
algorithms from having to worry about unbounded-length graphemes.  This in
turn allows the Unicode algorithms to use side buffers of a small and fixed
size to do their operations, which obviates the need for most memory
allocations.

For more info on the stream-safe format, see the appropriate [part of
UAX15](https://unicode.org/reports/tr15/#Stream_Safe_Text_Format).

## Unicode reference

See [UAX15 Unicode Normalization Forms](https://unicode.org/reports/tr15) for
more information on Unicode normalization.

# Use cases

## Case 1: Normalize a sequence of code points to NFC

We want to make a normalized copy of `s`, and we want the underlying
implementation to use a specialized UTF-8 version of the normalization
algorithm.  This is the most flexible and general-purpose API.

```cpp
std::string s = /* ... */; // using a std::string to store UTF-8
assert(!std::uc::is_normalized(std::uc::as_utf32(s)));

char8_t * nfc_s = new char8_t[s.size() * 2];
// Have to use as_utf32(), because normalization operates on code points, not UTF-8.
auto out = std::uc::normalize<std::uc::nf::c>(std::uc::as_utf32(s), nfc_s);
*out = '\0';
assert(std::uc::is_normalized(nfc_s, out));
```

## Case 2: Normalize a sequence of code points to NFC, where the output is going into a string-like container

This is like the previous case, except that the results must go into a
string-like container, not just any old output iterator.  The advantage of
doing things this way is that the code is a lot faster if you can append the
results in chunks.

```cpp
std::string s = /* ... */; // using a std::string to store UTF-8
assert(!std::uc::is_normalized(std::uc::as_utf32(s)));

std::string nfc_s;
nfc_s.reserve(s.size());
// Have to use as_utf32(), because normalization operates on code points, not UTF-8.
std::uc::normalize_append<std::uc::nf::c>(std::uc::as_utf32(s), nfc_s);
assert(std::uc::is_normalized(std::uc::as_utf32(nfc_s)));
```

## Case 3: Modify some normalized text without breaking normalization

You cannot modify arbitrary text that is already normalized without risking
breaking the normalization.  For instance, let's say I have some
NFC-normalized text.  That means that all the combining code points that could
combine with one or more preceding code points have already done so.  For
instance, if I see "ä" in the NFC text, then I know it's code point U+00E4
"Latin Small Letter A with Diaeresis", *not* some combination of "a" and a
combining two dots.

Now, forget about the "ä" I just gave as an example.  Let's say that I want to
insert a single code point, "¨̈" (U+0308 Combining Diaeresis) into NFC text.
Let's also say that the insertion position is right after a letter "o".  If I
do the insertion and then walk away, I would have broken the NFC normalization,
because "o" followed by "¨" is supposed to combine to form "ö" (U+00F6 Latin
Small Letter O with Diaeresis).

Similar things can happen when deleting text -- sometimes the deletion can
leave two code points next to each other that should interact in some way that
did not apply when they were separate, before the deletion.

```cpp
std::string s = /* ... */;                            // using a std::string to store UTF-8
assert(std::uc::is_normalized(std::uc::as_utf32(s))); // already normalized

std::string insertion = /* ... */;
normalize_insert<std::uc::nf::c>(s, s.begin() + 2, std::uc::as_utf32(insertion));
assert(std::uc::is_normalized(std::uc::as_utf32(nfc_s)));
```

# Proposed design

## Dependencies

This proposal depends on the existence of
[P2728](https://isocpp.org/files/papers/P2728R0.html) "Unicode in the Library,
Part 1: UTF Transcoding".

## Add Unicode version observers

```c++
namespace std::uc {
  inline constexpr major_version = @*implementation defined*@;
  inline constexpr minor_version = @*implementation defined*@;
  inline constexpr patch_version = @*implementation defined*@;
}
```

Unlike [P2728](https://isocpp.org/files/papers/P2728R0.html) (Unicode Part 1),
the interfaces in this proposal refer to parts of the Unicode standard that
are allowed to change over time.  The normalization of code points will not
change in Unicode N from what it was for those same code points in Unicode
N-1, but since new code points are introduced with each new Unicode release,
the normalization algorithms must be updated to keep up.

I'm proposing that implementations provide support for whatever version of
Unicode they like, as long as they document which one is supported via
`major_`-/`minor_`-/`patch_version`.

## Add stream-safe view

# TODO

### `stream_safe_view::iterator`

```c++
namespace std::uc {
  constexpr int @*uc-ccc*@(char32_t cp); // @*exposition only*@
}
```

`@*uc-ccc*@()` returns the [Canonical Combining
Class](https://unicode.org/reports/tr44/#Canonical_Combining_Class_Values),
which indicates how and whether a code point combines with other code points.
For some code point `cp`, `@*uc-ccc*@(cp) == 0` iff `cp` is a
"starter"/"noncombiner".  Any number of "nonstarters"/"combiners" may follow a
starter (remember that the purpose of the stream-safe format is to limit the
maximum number of combiners to at most 30).

The behavior of this iterator should be left to the implementation, as long as
the result meets the stream-safe format, and does not 1) change or remove any
starters, or 2) change the first 30 nonstarters after any given starter.  The
Unicode standard shows a technique for inserting special dummy-starters (that
do not interact with most other text) every 30 non-starters, so that the
original input is preserved.  I think this is silly -- the longest possible
meaningful sequence of nonstarters is 17 code points, and that is only
necessary for backwards comparability.  Most meaningful sequences are much
shorter.  I think a more reasonable implementation is simply to truncate any
sequence of nonstarters to 30 code points.

## Add stream-safe adaptor

# TODO

## Add an enumeration listing the supported normalization forms

`nf` is short for normalization form, and the letter(s) of each enumerator
indicate a form.  The Unicode normalization forms are NFC, NFD, NFKC, and
NFKD.  There is also an important semi-official one called FCC (described in
[Unicode Technical Note #5](https://unicode.org/notes/tn5)).

Using this enumeration, a user would spell NFD `std::uc::nf::d`.

```cpp
namespace std::uc {
  enum class nf {
    c,
    d,
    kc,
    kd,
    fcc
  };
```

## Add normalization views

# TODO

## Add normalization adaptors

# TODO

## Add a feature test macro

Add the feature test macro `__cpp_lib_unicode_normalization`.

# Implementation experience

All of these interfaces have been implemented in
[Boost.Text](https://github.com/tzlaine/text) (proposed -- not yet a part of
Boost).  All of the interfaces here have been very well-exercised by
full-coverage tests (millions of lines of code testing thousands of cases
published by Unicode), and by other parts of Boost.Text that use
normalization.
