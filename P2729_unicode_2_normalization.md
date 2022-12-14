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
  transcoding. This includes support for sentinels.

- Iterators are the present. We should support generic programming, whether it
  is done in terms of pointers, a particular iterator, or an iterator type
  specified as a template parameter.

- A null-terminated string should not be treated as a special case. The
  ubiquity of such strings means that they should be treated as first-class
  strings.

- If there's a specific algorithm specialization that operates directly on
  UTF-8 or UTF-16, the top-level algorithm should use that when appropriate.
  This is analogous to having multiple implementations of the algorithms in
  `std` that differ based on iterator category.

- Input may come from UTF-8, UTF-16, or UTF-32 strings (though UTF-32 is
  extremely uncommon in practice).  There should be a single overload of each
  normalization function, so that the user does not need to change code when
  the input is changed from UTF-N to UTF-M.  The most optimal version of the
  algorithm (processing either UTF-8 or UTF-16) will be selected (as mentioned
  above).

# The shortest Unicode normalization primer I can manage

You can have different strings of code points that mean the same thing.  For
example, you could have the code point "ä" (U+00E4 Latin Small Letter A with
Diaeresis), or you could have the two codepoints "a" (U+0061 Latin Small
Letter A) and "¨̈" (U+0308 Combining Diaeresis).  The former represents "ä" as
a single code point, the latter as two.  Unicode rules state that both strings
must be treated as identical.

To make such comparisons more efficient, Unicode has normalization forms.  If
all the text you ever compare is in the same normalization form, it doesn't
matter whether they're all the composed form like "ä" or the decomposed form
like "a¨" -- they'll all compare bitwise-equal to each other if they represent
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
(e.g. 'A') is followed by one or more combining code points (e.g. some number
of umlauts). It is valid to have an 'A' followed by 100 million umlauts. This
is valid but not useful. Unicode specifies something called the Stream-Safe
Format. This format inserts extra code points between combiners to ensure that
there are never more than 30 combiners in a row. In practice, you should never
need anywhere near 30 to represent meaningful text.

Long sequences of combining characters create a problem for algorithms like
normalization and grapheme breaking; the grapheme breaking algorithm may be
required to look ahead a very long way in order to determine how to handle the
current grapheme. To address this, Unicode allows a conforming implementation
to assume that a sequence of code points contains graphemes of at most 31 code
points. This is known as the Stream-Safe Format assumption.  All the proposed
interfaces here and in the papers to come make this assumption.

# Use cases

## Case 1: Normalize a sequence of code points to NFC

We want to make a normalized copy of `s`, and we want the underlying
implementation to use a specialized UTF-8 version of the normalization
algorithm.  This is the most flexible and general-purpose API.

```cpp
std::string s = /* ... */; // using a std::string to store UTF-8
assert(!std::uc::is_normalized(std::uc::as_utf32(s)));

char * nfc_s = new char[s.size() * 2];
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
combine with one or more preceeding code points have already done so.  For
instance, if I see "ä" in the NFC text, then I know it's code point U+00E4
"Latin Small Letter A with Diaeresis", *not* some combination of "a" and a
combining two dots.

Now, forget about the "ä" I just gave as an example.  Let's say that I want to
insert a single code point, "¨̈" (U+0308 Combining Diaeresis) into NFC text.
Let's also say that the insertion position is right after a letter "o".  If I
do the insertion and then walk away, I would have broken the NFC normaliztion,
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

## Add concepts that describe the constraints on parameters to the normalization API

```cpp
namespace std::uc {
  template<class T, class CodeUnit>
  concept @*eraseable-insertable-sized-bidi-range*@ = // @*exposition only*@
    ranges::sized_range<T> &&
    ranges::bidirectional_range<T> &&
    requires(T t, const CodeUnit* it) {
      { t.erase(t.begin(), t.end()) } -> same_as<ranges::iterator_t<T>>;
      { t.insert(t.end(), it, it) } -> same_as<ranges::iterator_t<T>>;
    };

  template<class T>
    concept utf8_string =
      utf8_code_unit<ranges::range_value_t<T>> &&
      @*eraseable-insertable-sized-bidi-range*@<T, ranges::range_value_t<T>>;

  template<class T>
    concept utf16_string =
      utf8_code_unit<ranges::range_value_t<T>> &&
      @*eraseable-insertable-sized-bidi-range*@<T, ranges::range_value_t<T>>;

  template<class T>
    concept utf_string = utf8_string<T> || utf16_string<T>;
}
```

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

## Add a generic normalization algorithm

`normalize()` takes some input in code points, and writes the result to the
out iterator `out`, also in code points.  Since there are fast implementations
that normalize UTF-16 and UTF-8 sequences, if the user passes a UTF-8 ->
UTF-32 or UTF-16 -> UTF-32 transcoding iterator to `normalize()`, it is
allowed to get the underlying iterators out of `[first, last)`, and do all the
normalization in UTF-8 or UTF-16.

You may expect `normalize()` to return an alias of `in_out_result`, like
`std::ranges::copy()`, or `std::uc::transcode_to_utf8()` from
[P2728](https://isocpp.org/files/papers/P2728R0.html).  The reason it does not
is that it may use `first` and `last`, or it may use the underlying iterators
that `first` and/or `last` adapt.  If it uses the latter, the information
about how to construct an iterator of type `I` that represents the final
position of the input is already lost.

```cpp
  template<nf Normalization, utf_iter I, sentinel_for<I> S, output_iterator<uint32_t> O>
    O normalize(I first, S last, O out);

  template<nf Normalization, utf_range_like R, output_iterator<uint32_t> O>
    O normalize(R&& r, O out);

  template<nf Normalization, utf_iter I, sentinel_for<I> S>
    bool is_normalized(I first, S last);

  template<nf Normalization, utf_range_like R>
    bool is_normalized(R&& r);
```

## Add an append version of the normalization algorithm

In performance tests, I found that appending multiple elements to the output
in one go was substantially faster than the more generic `normalize()`
algorithm, which appends to the output one code point at a time.  So, we
should provide support for that as well, in the form of `normalize_append()`.

If transocding is necessary when the result is appended, `normalize_append()`
does automatic transcoding to UTF-N, where N is implied by the size of
`String::value_type`.

```cpp
  template<nf Normalization, utf_iter I, sentinel_for<I> S, utf_string String>
    void normalize_append(I first, S last, String& s);

  template<nf Normalization, utf_range_like R, utf_string String>
    void normalize_append(R&& r, String& s);

  template<nf Normalization, utf_string String>
    void normalize_string(String& s);
```

## Add normalization-aware insertion, erasure, and replacement operations on strings

If you need to insert text into a `std::string` or other STL-compatible
container, you can use the erase/insert/replace API.  There are iterator and
range overloads of each. Each one:

- normalizes the inserted text (if text is being inserted);
- places the inserted text in Stream-Safe Format (if text is being inserted);
- performs the erase/insert/replace operation on the string;
- ensures that the result is in Stream-Safe Format (if text is being erased); and
- normalizes the code points on either side of the affected subsequence within the string.

This last step is necessary because insertions and erasures may create
situations in which code points which may combine are now next to each other,
when they were not before.  It's all very complicated, and the user should
have a means of doing this generically, and remaining ignorant of the details.

This API is like the `normalize_append()` overloads in that it may operate on
UTF-8 or UTF-16 containers, and deduces the output UTF from the size of the
mutated container's `value_type`.

About the need for `replace_result`: `replace_result` represents the result of
inserting a sequence of code points `I` into an existing sequence of code
points `E`, ensuring proper normalization.  Since the insertion operation may
need to change some code points just before and/or just after the insertion
due to normalization, the code points described by `replace_result` may be
longer than `I`.  `replace_result` values represent the entire sequence of
code points in `E` that have changed -- some version of which may have already
been present in the string before the insertion.

Note that `replace_result::iterator` refers to the underlying sequence, which
may not itself be a sequence of code points.  For example, the underlying
sequence may be a sequence of `char` which is interpreted as UTF-8.  We can't
return an iterator of the type `I` passed to `normalize_replace()`, for the
same reason we don't return an `in_out_result` from `normalization()`.

```cpp
  template<class I>
  struct replace_result : subrange<I> {
    using iterator = I;

    replace_result() = default;
    replace_result(iterator first, iterator last) : subrange<I>(first, last) {}

    operator iterator() const { return this->begin(); }
  };

  enum insertion_normalization {
    insertion_normalized,
    insertion_not_normalized
  };

  template<
    nf Normalization,
    utf_string String,
    code_point_iter I,
    class StringIter = ranges::iterator_t<String>>
  replace_result<StringIter> normalize_replace(
    String& string,
    StringIter str_first,
    StringIter str_last,
    I first,
    I last,
    insertion_normalization insertion_norm = insertion_not_normalized);

  template<
    nf Normalization,
    utf_string String,
    code_point_iter I,
    class StringIter = ranges::iterator_t<String>>
  replace_result<StringIter> normalize_insert(
    String& string,
    StringIter at,
    I first,
    I last,
    insertion_normalization insertion_norm = insertion_not_normalized);

  template<
    nf Normalization,
    utf_string String,
    code_point_range R,
    class StringIter = ranges::iterator_t<String>>
  replace_result<StringIter> normalize_insert(
    String& string,
    StringIter at,
    R&& r,
    insertion_normalization insertion_norm = insertion_not_normalized);

  template<
    nf Normalization,
    utf_string String,
    class StringIter = ranges::iterator_t<String>>
  replace_result<StringIter> normalize_erase(
    String& string, StringIter str_first, StringIter str_last);
}
```

## Design notes

Unlike the interfaces from
[P2728](https://isocpp.org/files/papers/P2728R0.html) "Unicode in the Library,
Part 1: UTF Transcoding", there are no pointer overloads of any of these
interfaces.  This is because the pointer-as-null-terminated-range notion does
not apply here.  Instead of taking any kind of UTF as input, the normalization
APIs require code points as input.  Null-terminated strings of UTF-32 are not
a thing.

# Implementation experience

TODO

# Performance

TODO

