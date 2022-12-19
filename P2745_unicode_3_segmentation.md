---
title: "Unicode in the Library, Part 3: Text Segmentation"
document: D2745R0
date: 2022-12-14
audience:
  - SG-16 Unicode
  - LEWG-I
  - LEWG
author:
  - name: Corentin Jabot
    email: <corentin.jabot@gmail.com>
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: true
monofont: "DejaVu Sans Mono"

---

# Motivation

TODO

- Importance of UTF-8
- Importance of graphemes
- Graphemes should not be tailorable

# The shortest Unicode text segmentation primer you'll read this week

## Graphemes

Unicode has a notion called an Extended Grapheme Cluster.  This is usually
just referred to as a "grapheme" for short.  A grapheme is a sequence of code
points that go together in some way that makes it useful to think about them
as a single thing.  For instance, consider "a¨", consisting of the two code
points U+0061 Latin Small Letter A and U+0308 Combining Diaeresis.  These two
code points form a single grapheme.  Many sequences of emoji that combine to
form a single symbol that the reader sees will also form a single grapheme.
For instance, if you combine an emoji that contains a face with a
colored-square emoji, the reader should see a single face in the color
indicated by the square -- readers are not supposed to see two emoji, one of
which is a colored square.

A grapheme is thus the closest thing to what the end user (the reader of the
text) thinks of as a "character".  After all, the reader doesn't ever see
"a¨", even though that's what exists in memory -- instead, she sees "ä".  For
this reason, graphemes -- not code points or code units -- are a very natural
unit of work for programmers working with end-user-facing text.

## Text Segmentation

Unicode provides algorithms that break up text into segments consisting of
graphemes, words, sentences, and lines.  Each segment is a pair of "breaks".
A break is the code point just after a boundary between two segments.  For
example, the word break algorithm would take the text `"The quick (“brown”)
fox"`, and would produce this sequence of segments:

```c++
{"The", " ", "quick", " ", "(", "“", "brown", "”", ")", " ", "fox"}
```

Notice that those are not all what we normally think of as words.  In
particular, sequences of whitespace in between what we normally think of as
words are also counted as words.

Each segmentation/breaking algorithm works by looking up the
algorithm-specific property for each code point that is processed, and then
determining where breaks exist by looking at the sequence of properties.  For
instance, in the word breaking algorithm, "a\\nb" would have the properties
{ALetter, Newline, ALetter}, resulting in the detection of 3 words, "a",
"\\n", and "b".  By contrast, "ab\\n" would have the properties {ALetter,
ALetter, Newline}, resulting in the detection of 2 words, "ab" and "\\n".

Each segmentation algorithm operates on what is effectively a projection of
input code points to properties.

## Tailoring

Some of the test segmentation algorithms are tailorable.  This means that they
allow the user to adjust how aspects of the segmentation is performed.  For
instance, some languages use a space instead of `'.'` or `','` as a
thousands-separator, as in `"1 234,56"`.  For the word break algorithm to
treat that string as a single word, you would need to tailor the word break
algorithm to treat a space as a valid element of a number if it appears
*within* the number.  Tailoring is really important in word breaking; we've
all seen word breaking that makes different choices in different places.  For
instance, the word breaks in you favorite shell are probably different from
the word breaks in your favorite editor, Emacs.  A couple more examples:
Should `"l'objectif"` be one word, or three?  Should "root-cause" be one word
or three?

## Unicode reference

See [UAX29, Unicode Text Segmentation](https://unicode.org/reports/tr29) for
more information.

# Use cases

## Case 1: TODO

TODO

# Proposed design

## Dependencies

This proposal depends on the existence of
[P2729](https://isocpp.org/files/papers/P2729R0.html) "Unicode in the Library,
Part 2: Normalization".

## Add interfaces for grapheme breaking

First, we need to add an enumeration of the grapheme properties:

```c++
namespace std::uc {
  enum class grapheme_property { @*implementation defined*@ };
}
```

The enumeration should have the same values in it that appear in [UAX29, Table
2. Grapheme_Cluster_Break Property
Values](https://unicode.org/reports/tr29/#Grapheme_Cluster_Break_Property_Values),
with two modifications.  First, the "Any" property should be called `Other`,
and should appear as the first enumerator, with the value `0`.  Second, the
values in the table whose descriptions read "This value is obsolete and
unused." may be excluded.  Except for the Any property, the enumerator names
should match the names in UAX29 exactly, including case.

For example, for Unicode 11, this enumeration might be:

```c++
namespace std::uc {
  enum class grapheme_property {
    Other,
    CR,
    LF,
    Control,
    Extend,
    Regional_Indicator,
    Prepend,
    SpacingMark,
    L,
    V,
    T,
    LV,
    LVT,
    ExtPict,
    ZWJ
  };
}
```

Next, we need a function that maps code points to `grapheme_property`s:

```c++
namespace std::uc {
  grapheme_property grapheme_prop(uint32_t cp);
}
```

Next, we need functions that detect the locations of grapheme breaks:

```c++
namespace std::uc {
  template<class T>
    using @*range-like-iterator*@ = @*see below*@;  // @*exposition only*@

  template<utf_iter I, sentinel_for<I> S>
    I prev_grapheme_break(I first, I it, S last);

  template<utf_range_like R>
  @*uc-result-iterator*@<R>
    prev_grapheme_break(R && r, @*range-like-iterator*@<R> it);

  template<utf_iter I, sentinel_for<I> S>
  I next_grapheme_break(I first, S last);

  template<utf_range_like R>
    @*uc-result-iterator*@<R>
      next_grapheme_break(R && r, @*range-like-iterator*@<R> it);

  template<utf_iter I, sentinel_for<I> S>
    bool at_grapheme_break(I first, I it, S last);

  template<utf_range_like R>
    bool at_grapheme_break(R && r, @*range-like-iterator*@<R> it);
}
```

Note that `@*uc-result-iterator*@<R>` comes from
[P2728](https://isocpp.org/files/papers/P2728R0.html).  It provides a
`ranges::borrowed_iterator_t<R>` or just a pointer, as appropriate, based `R`.

`@*range-like-iterator*@<T>` is `T` if `is_pointer_v<remove_reference_t<T>>`
is `true`, and `ranges::iterator_t<T>` otherwise.

`prev_grapheme_break(range, it)` returns `it` if `it` is already at a break,
or the break before `it` otherwise. There is one exception to this — even
though there is always an implicit break at the end of a sequence of code
points, if `it == range.end()`, the previous break is still returned, if any.

This behavior allows us to do two convenient things with
`prev_grapheme_break()`. First, we can use `prev_grapheme_break(first, it,
last) == it` as a predicate that `it` is at a break. Second, we can use
`prev_grapheme_break()` followed by `next_grapheme_break()` to find the
nearest breaks around `it`.

`next_grapheme_break()` returns the next break after the given iterator. It
has a precondition that the given iterator is already at a grapheme break.

## Add classes for holding/referring to graphemes

### Reference class template `grapheme_ref`

```c++
namespace std::uc {
  template<code_point_iter I>
  struct grapheme_ref : utf32_view<I>
  {
    constexpr grapheme_ref() = default;
    constexpr grapheme_ref(I first, I last)
      : utf32_view<I>(first, last) {}
    constexpr grapheme_ref(utf32_view<I> r)
      : grapheme_ref(r.begin(), r.end()) {}
    constexpr grapheme_ref(const grapheme& g)
      requires same_as<I, grapheme::iterator>
        : utf32_view<I>(g.begin(), g.end()) {}

    friend constexpr bool operator==(grapheme_ref lhs, grapheme_ref rhs)
      { return ranges::equal(lhs, rhs); }
  };

  template<class I>
   grapheme_ref(I, I) -> grapheme_ref<I>;

  template<class I>
   grapheme_ref(utf32_view<I>) -> grapheme_ref<I>;

  grapheme_ref(grapheme) -> grapheme_ref<grapheme::const_iterator>;

  template<code_point_iter I1, code_point_iter I2>
   constexpr bool operator==(grapheme_ref<I1> lhs, grapheme_ref<I2> rhs)
    { return ranges::equal(lhs, rhs); }
}

namespace std::ranges {
  template<class I>
    inline constexpr bool enable_borrowed_range<uc::grapheme_ref<I>> = true;
}
```

`grapheme_ref` is a non-owning reference to a sequence of code units that
comprisse a single grapheme; each the constructors has a precondition that the
sequence of code points it is constructed with comprise a single grapheme.
`grapheme_ref` is comparable with a `grapheme` (see below), and
`grapheme_ref`s instantiated with different `I` template parameters.

Construction from `utf32_view`s and `grapheme`s is intentionally left
implicit.

`grapheme_ref` is used as the `reference` type for `grapheme_iterator`.

### Owning class `grapheme`

```c++
namespace std::uc {
  struct grapheme
  {
    using iterator = utf_8_to_32_iterator<const char*>;
    using const_iterator = iterator;

    grapheme() {}
    template<utf_iter I, sentinel_for<I> S>
      grapheme(I first, S last);
    grapheme(uint32_t cp);
    template<utf_range_like R>
      grapheme(R&& r);

    bool empty() const;
    size_t distance() const;

    const_iterator begin() const;
    const_iterator end() const;

    bool operator==(const grapheme& other) const;

    template<class CharT, class Traits>
      friend ostream<CharT, Traits>&
        operator<<(ostream<CharT, Traits>& os, const grapheme& g);
  };
}
```

`grapheme` provides a type with storage to hold a grapheme.  It is mostly
immutable; its only mutating operations are move and assignment.  Using a
small buffer optimization is pretty important for this type, since the vast
majority of graphemes will only be a few bytes.  Also, its internal storage is
a UTF-8 sequence of `char`, as implied by the `iterator` type.  It is expected
that most graphemes will be processed as `grapheme_ref`s, and `grapheme`
exists simply to provide a value-semantic alternative when that is called for.

## Add `grapheme_iterator`

```c++
namespace std::uc {
    template<code_point_iter I, sentinel_for<I> S = I>
    struct grapheme_iterator
    {
        using value_type = grapheme_ref<I>;
        using difference_type = ptrdiff_t;
        using pointer = proxy_arrow_result<value_type>;
        using reference = value_type;
        using iterator_category = bidirectional_iterator_tag;

        using iterator = I;
        using sentinel = S;

        constexpr grapheme_iterator() = default;
        constexpr grapheme_iterator(iterator first, iterator it, sentinel last);
        template<code_point_iter I2, sentinel_for<I2> S2>
          requires convertible_to<I2, I> && convertible_to<S2, S>
            constexpr grapheme_iterator(grapheme_iterator<I2, S2> const & other);

        constexpr reference operator*() const;
        constexpr pointer operator->() const;

        constexpr iterator base() const;

        constexpr grapheme_iterator & operator++();
        constexpr grapheme_iterator operator++(int);

        constexpr grapheme_iterator & operator--();
        constexpr grapheme_iterator operator--(int);

        friend constexpr bool operator==(grapheme_iterator lhs, grapheme_iterator rhs)
          { return lhs.base() == rhs.base(); }
        friend constexpr bool operator==(grapheme_iterator lhs, sentinel rhs)
          { lhs.base() == rhs; }

    private:
        template<code_point_iter I2, sentinel_for<I2> S2>
          friend struct grapheme_iterator;
    };

    template<class Iter1, class Sentinel1, class Iter2, class Sentinel2>
      requires same_as<Sentinel1, null_sentinel_t> != same_as<Sentinel2, null_sentinel_t>
    constexpr auto operator==(
      const grapheme_iterator<Iter1, Sentinel1>& lhs,
      const grapheme_iterator<Iter2, Sentinel2>& rhs) -> decltype(lhs.base() == rhs.base())
        { return lhs.base() == rhs.base(); }
}
```


TODO: formatters!

## The pattern of all text breaking algorithms

All the remaining text breaking interfaces follow the pattern described above
for grapheme breaking.  Each kind of break `X` has:

- an enumeration of the relevant properties, `enum class X_property`, and an associated
  lookup function `X_property X_prop(uint32_t)`;
- iterator and range forms of `prev_X_break()`;
- iterator and range forms of `next_X_break()`; and
- iterator and range forms of `at_X_break()`.

Tailoring is available for some of the kinds of breaking that follow, and when
it is in play, the tailoring parameters will always come in the form of
defaulted parameters that follow the ones for the grapheme breaking functions.

## Add `grapheme_view` and `as_graphemes()`

```c++
namespace std::us {
  template<class I, class S>
    using @*grapheme-view-sentinel*@ = @*see below*@; // @*exposition only*@

  template<code_point_iter I, sentinel_for<I> S = I>
  struct grapheme_view : view_interface<grapheme_view<I, S>>
  {
    using iterator = grapheme_iterator<I, S>;
    using sentinel = @*grapheme-view-sentinel*@<I, S>;

    constexpr grapheme_view() : first_(), last_() {}

    constexpr grapheme_view(iterator first, sentinel last) :
      first_(first), last_(last) {}

    constexpr grapheme_view(I first, S last)
      : first_(first, first, last), last_(@*make-last*@<sentinel>(first, last)) {}

    template<code_point_iter I2>
      requires constructible_from<iterator, I2, I2, I2> &&
        constructible_from<sentinel, I2, I2, I2>
    constexpr grapheme_view(
      I2 first, I2 view_first, I2 view_last, I2 last)
        : first_(first, view_first, last), last_(first, view_last, last) {}

    constexpr iterator begin() const { return first_; }
    constexpr sentinel end() const { return last_; }

    friend constexpr bool operator==(grapheme_view lhs, grapheme_view rhs)
      { return lhs.begin() == rhs.begin() && lhs.end() == rhs.end(); }

    template<class CharT, class Traits>
      friend ostream<CharT, Traits>&
        operator<<(ostream<CharT, Traits>& os, grapheme_view v);

  private:
    template<class ResultType, class I1, class I2, class S>
      static auto @*make-last*@(I1 first, I2 it, S last) { // @*exposition only*@
        if constexpr (requires { ResultType(first, it, last); }) {
          return ResultType{first, it, last};
        } else {
          return it;
        }
      }

    iterator first_;                      // @*exposition only*@
    [[no_unique_address]] sentinel last_; // @*exposition only*@
  };

  template<class I, class S>
    grapheme_view(grapheme_iterator<I, S>, grapheme_iterator<I, S>) -> grapheme_view<I, S>;

  template<class I, class S>
    grapheme_view(grapheme_iterator<I, S>, S) -> grapheme_view<I, S>;

  template<class I, class S>
    grapheme_view(I, S) -> grapheme_view<I, S>;

  template<utf_iter I, std::sentinel_for<I> S>
    constexpr @*unspecified*@ as_graphemes(I first, S last);

  template<utf_range_like R>
    constexpr @*unspecified*@ as_graphemes(R&& r);

  struct @*as-graphemes-t*@ : range_adaptor_closure<@*as-graphemes-t*@> { // @*exposition only*@
    template<utf_iter I, std::sentinel_for<I> S>
      constexpr @*unspecified*@ operator()(I first, S last) const;
    template<utf_range_like R>
      constexpr @*unspecified*@ operator()(R && r) const;
  };

  inline constexpr @*as-graphemes-t*@ as_graphemes;
}
```

`@*grapheme-view-sentinel*@<I, S>` is `grapheme_iterator<I, S>` if `is_same<I,
S>`, and `S` otherwise.

The reason for the fourth `grapheme_view` constructor (the constrained one) is
that is is more flexible in some cases when iterators from the `grapheme_view`
need to be compared to other iterators.  The next section explains why.

`@*as-graphemes-t*@::operator()` each return a `grapheme_view` of the
appropriate type, except that the range overload returns `ranges::dangling{}`
if `!is_pointer_v<remove_reference_t<R>> && !ranges::borrowed_range<R>` is
`true`.

`as_graphemes` can also be used as a range adaptor, as in `r |
std::uc::as_graphemes`.

### An important note about comparability of grapheme and transcoding iterators

Often, iterators from subranges can only be compared to each other.  This
follows directly from the fact that all transcoding and grapheme iterators
have a built-in end point (see [P2728's explaination of
this](https://isocpp.org/files/papers/P2728R0.html#design-notes); look at the
last paragraph). Say you get a subrange from one iteration of a text
segmentation algorithm:

```c++
char const * c_str = /* ... */;
auto const lines = c_str | std::uc::as_graphemes | std::uc::lines(std::uc::allowed_breaks);

int line_index = 0;
for (auto line : lines) {
    auto first = lines.begin()->begin();
    std::cout << "line " << line_index++ << " offsets: "
              << std::ranges::distance(first, line.begin())
              << " - "
              << std::ranges::distance(first, line.end()) // Oops.
              << "\n";
}
```

This code does not halt. The line marked with "Oops." will continue to count
forever when it is executed in the second loop iteration. This happens because
`first` is constructed from the iterators delimiting the first line,
`*lines.begin()` (let's call that line `l` for brevity). `first`'s underlying
iterators are:

- `l.begin().base()`, `first`'s lower bound, which points to the first code
  point in `l`;

- `l.begin().base()`, which is the current position of
  `first` within `l`; and

- `l.end().base()`, `first`'s upper bound, or one past the last code point in
  `l`.

When evaluating `std::ranges::distance(first, line.end())`, `first` must be
advanced until it is equal to `line.end()`. However, there is an upper bound
on how far we can advance `first`. It cannot advance past its underlying
upper-bound iterator, which is equal to `l.end().base()` (which is
`lines.begin()->end().base()`). This upper bound will always be less than
`line.end()`. Remember, the line in `line.end()` is the line in the *second*
iteration of the loop, but the line `l` (`== *lines.begin()`) is the line in
the *first* iteration of the loop.

That was complicated; to keep things simple, users can follow this rule:

When given a grapheme or transcoding subrange `s`, comparisons of `s.begin()`
to `s.end()` are fine, and so is iteration between `s.begin()` and
`s.end()`. However, iteration between either `s.begin()` or `s.end()` and any
other iterator may result in undefined behavior.

## An additional pattern for all non-grapheme text breaking interfaces

In addition to the pattern established by grapheme breaking interfaces, each
other kind of break `X` has:

- `X()`, which returns the smallest range of code points that comprise an `X`
  (word, line, etc.) in which it is found; and

- iterator and range forms of `Xs()`, which returns a view of subranges. Each
  subrange is an `X`.

`Xs` is inovocable, but it is also a view adaptor, and can be used in pipe
expressions -- without parameters -- as in `r | std::uc::lines`.

Since all the text segmentation operations can be done in in terms of next and
previous steps, `Xs` can be reversed, as in `r | std::uc::words |
std::views::reverse`.

`X` and the `Xs` all return `std::ranges::dangling{}` when given a
`utf_range_like` `R` for which `!is_pointer_v<remove_reference_t<R>> &&
!ranges::borrowed_range<R>` is `true`.

As mentioned previously, the template and function parameters above may be
altered slightly, as tailoring needs dictate.

### Add `break_view`

The view created by using `words`, `lines`, etc., is a `break_view`.
`break_view` represents a generic text segmentation, in that it is agnostic to
the kind of segmentation it represents; it represents whatever segmentation
the `PrevFunc` and `NextFunc` invocables produce.

```c++
namespace std::uc {
  template<code_point_iter I, sentinel_for<I> S, class PrevFunc, class NextFunc, class SubRange>
  struct break_iterator
    : proxy_iterator_interface<
        break_iterator<I, S, PrevFunc, NextFunc, Subrange>,
        bidirectional_iterator_tag,
        Subrange> {
    break_iterator() = default;
    break_iterator(I first, S last, PrevFunc* prev_func, NextFunc* next_func);
    break_iterator(I first, I it, S last, PrevFunc* prev_func, NextFunc* next_func)
      : first_(first), seg_(it, it), last_(last), prev_func_(prev_func), next_func_(next_func) {}

    Subrange operator*() const { return Subrange{seg_.first, seg_.second}; }

    break_iterator& operator++();
    break_iterator& operator--();

    friend bool operator==(break_iterator lhs, break_iterator rhs)
      { return lhs.seg_ == rhs.seg_; }

    friend bool operator==(break_iterator lhs, S rhs)
      requires !same_as<I, S>
        { return lhs.seg_.first == rhs; }

    using base_type = proxy_iterator_interface< // @*exposition only*@
      break_iterator<I, S, PrevFunc, NextFunc, Subrange>,
      bidirectional_iterator_tag,
      Subrange>;
    using base_type::operator++;
    using base_type::operator--;

  private:
    I first_ = {};                      // @*exposition only*@
    pair<I, I> seg_ = {};               // @*exposition only*@
    [[no_unique_address]] S last_ = {}; // @*exposition only*@
    PrevFunc* prev_func_ = nullptr;     // @*exposition only*@
    NextFunc* next_func_ = nullptr;     // @*exposition only*@
  };

  template<
    code_point_iter I, sentinel_for<I> S,
    invocable<I, I, S> PrevFunc, invocable<I, S> NextFunc,
    class Subrange = utf32_view<I>>
  struct break_view : view_interface<break_view<I, S, PrevFunc, NextFunc, Subrange>> {
    using iterator = break_iterator<I, S, PrevFunc, NextFunc, Subrange>;
    using sentinel = @*see below*@;

    break_view() {}
    break_view(I first, S last, PrevFunc prev_func, NextFunc next_func);

    iterator begin() const { return first_; }
    sentinel end() const { return last_; }

    PrevFunc&& prev_func() && { return std::move(prev_func_); }
    NextFunc&& next_func() && { return std::move(next_func_); }

    template<class CharT, class Traits>
      friend ostream<CharT, Traits>&
        operator<<(ostream<CharT, Traits>& os, break_view bv);

  private:
    iterator first_;                      // @*exposition only*@
    [[no_unique_address]] sentinel last_; // @*exposition only*@
    PrevFunc prev_func_;                  // @*exposition only*@
    NextFunc next_func_;                  // @*exposition only*@
  };
}
```

`break_iterator` advances its subrange `seg_` forward or backward, by calling
`(*next_func_)` and `(*prev_func_)`, respectively, and produces the current
subrange by constructing a `Subrange` using the elements of `seg_`.

`break_view<I, S, ...>::sentinel` is `break_view<I, S, ...>::iterator` if
`is_same<I, S>`, and `S` otherwise.

### Why grapheme breaking does not follow the general pattern

The "additional pattern" descibed above does not apply to graphemes.
Graphemes are more like the UTFs than words, lines, and sentences.  Sine they
represent a single end-user perception of a "charater", graphemes are a unit
of work when processing text, but they are not individually very semantically
meaningful, like a word or a line.  The operation that produces the set of all
graphemes in some range of text is therefore called `as_graphemes`, which
matches the naming of `as_utfN`, rather than `graphemes`, which would be
closer to the naming of `words`, `lines`, and `sentences`.

In particular, finding the grapheme around a particular point in text is a
somewhat uncommon operation, as opposed to finding the word or line that
contains a particular point in the text.  Not following the pattern of `word`,
`line`, etc., frees up `grapheme` to be used as the name of the value type for
holding a grapheme.

## Add interfaces for word breaking

These interfaces implement the Unicode word-breaking algorithm.

First, we need a couple of concepts to constrain the invocables that may be
used to tailor word breaking:

```c++
namespace std::uc {
  template<class T>
  concept word_prop_func = invocable<T, uint32_t> &&
    convertible_to<invoke_result_t<T, uint32_t>, word_property>;

  template<class T>
  concept word_break_func =
    invocable<T, uint32_t, uint32_t, uint32_t, uint32_t, uint32_t> &&
      convertible_to<
        invoke_result_t<T, uint32_t, uint32_t, uint32_t, uint32_t, uint32_t>,
        bool>;
}
```

Then we add the rest of the API:

```c++
namespace std::uc {
  enum class word_property { @*implementation defined*@ };
  word_property word_prop(uint32_t cp);

  struct untailored_word_prop {
    static word_property operator()(uint32_t cp)
      { return uc::word_prop(cp); }
  };

  struct untailored_word_break {
    static bool operator()(uint32_t, uint32_t, uint32_t, uint32_t, uint32_t)
      { return false; }
  };

  template<
    utf_iter I, sentinel_for<I> S,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  I prev_word_break(
    I first, I it, S last,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  template<
    utf_range_like R,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  @*uc-result-iterator*@ prev_word_break(
    R&& r,
    @*range-like-iterator*@<R> it,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  template<
    utf_iter I, sentinel_for<I> S,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  I next_word_break(
    I first, S last,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  template<
    utf_range_like R,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  @*uc-result-iterator*@ next_word_break(
    R&& r,
    std::ranges::iterator_t<R> it,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  template<
    utf_iter I, sentinel_for<I> S,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  bool at_word_break(
    I first, I it, S last,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  template<
    utf_range_like R,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  bool at_word_break(
    R&& r,
    @*range-like-iterator*@<R> it,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  template<
    utf_iter I, sentinel_for<I> S,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  utf32_view<I> word(
    I first, I it, S last,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  template<
    utf_range_like R,
    word_prop_func WordPropFunc = untailored_word_prop,
    word_break_func WordBreakFunc = untailored_word_break>
  utf32_view<ranges::iterator_t<R>> word(
    R&& r,
    @*range-like-iterator*@<R> it,
    const WordPropFunc& word_prop = WordPropFunc{},
    const WordBreakFunc& word_break = WordBreakFunc{});

  struct @*words-t*@ : range_adaptor_closure<@*words-t*@> { // @*exposition only*@
    template<
      utf_iter I, sentinel_for<I> S,
      word_prop_func WordPropFunc = untailored_word_prop,
      word_break_func WordBreakFunc = untailored_word_break>
    @*unspecified*@ operator()(
      I first, S last,
      const WordPropFunc& word_prop = WordPropFunc{},
      const WordBreakFunc& word_break = WordBreakFunc{}) const;

    template<
      utf_range_like R,
      word_prop_func WordPropFunc = untailored_word_prop,
      word_break_func WordBreakFunc = untailored_word_break>
    @*unspecified*@ operator()(
      R&& r,
      const WordPropFunc& word_prop = WordPropFunc{},
      const WordBreakFunc& word_break = WordBreakFunc{}) const;
  };

  inline constexpr @*words-t*@ words;
}
```

These operations have the semantics previously described.

### Tailoring word breaking

There are two ways to tailor word breaking.

As described above, each break algorithm is defined in terms of code point
properties; each code point is a letter, digit, punctuation, etc. All the word
break functions accept an optional word property lookup invocable `word_prop`
to replace the default one.

For example, here we've made a custom word property lookup function that treats
a regular dash `'-'` as a MidLetter. MidLetter is a property that repesents
code points that are part of a word as long as it can reach at least one
letter on either side, before reaching a word break first:

```c++
std::string cps("out-of-the-box");

// Prints "out/-/of/-/the/-/box/".
for (auto range : std::uc::words(cps)) {
    std::cout << range << "/";
}
std::cout << "\n";

auto const custom_word_prop = [](uint32_t cp) {
    if (cp == '-')
        return std::uc::word_property::MidLetter; // '-' becomes a MidLetter
    return std::uc::word_prop(cp);                // Otherwise, just use the default implementation.
};

// Prints "out-of-the-box/".
for (auto range : std::uc::words(cps, custom_word_prop)) {
    std::cout << range << "/";
}
std::cout << "\n";
```

Tailoring the properties for each code point works for some cases, but using
tailorings of the meanings of MidLetter and MidNum can only add to the sizes
of words; it cannot decrease their sizes. The word break functions take a
second optional parameter that allows you to pick arbitrary word breaks based
on limited context.

The word break algorithm uses the current code point, plus two code points
before and two code points after, to determine whether a word break exists at
the current code point. Therefore, the signature of the custom word break
function is this:

```c++
bool custom_break(uint32_t prev_prev,
                  uint32_t prev,
                  uint32_t curr,
                  uint32_t next,
                  uint32_t next_next);
```

Returning `true` indicates that [`prev`, `curr`] straddles a word break —
`prev` is the last code point of one word, and `curr` is the first code point
of the next. If provided, this custom break function is evaluated before any
of the Unicode word break rules.

```c++
std::string cps("snake_case camelCase");

// Prints "snake_case   camelCase ".
for (auto range : std::uc::words(cps)) {
    std::cout << range << " ";
}
std::cout << "\n";

// Break up words into chunks as if they were parts of identifiers in a
// popular programming language.
auto const identifier_break = [](uint32_t prev_prev,
                                 uint32_t prev,
                                 uint32_t curr,
                                 uint32_t next,
                                 uint32_t next_next) {
    if ((prev == '_') != (curr == '_'))
        return true;
    if (0x61 <= prev && prev <= 0x7a && 0x41 <= curr && curr <= 0x5a)
        return true;
    return false;
};

// Prints "snake _ case   camel Case ".
for (auto range : std::uc::words(cps, std::uc::word_prop, identifier_break)) {
    std::cout << range << " ";
}
std::cout << "\n";
```

### Limitations of the default word breaking algorithm

This algorithm does not work for all languages. From [UAX29, Unicode Text
Segmentation](https://unicode.org/reports/tr29):

>  For Thai, Lao, Khmer, Myanmar, and other scripts that do not typically use
>  spaces between words, a good implementation should not depend on the
>  default word boundary specification. It should use a more sophisticated
>  mechanism, as is also required for line breaking. Ideographic scripts such
>  as Japanese and Chinese are even more complex. Where Hangul text is written
>  without spaces, the same applies. However, in the absence of a more
>  sophisticated mechanism, the rules specified in this annex supply a
>  well-defined default.

French and Italian words are meant to be broken after an apostrophe, but the
default algorithm finds "l’objectif" to be a single word.

Breaking on dashes is the default. For example, the default algorithm finds
"out-of-the-box" to be seven words.

There are other, rarer failure cases in that document you might want to look
at too.  Implementations should be encouraged to cover as many of the word
breaking cases listed in UAX29 as possible.

## Add interfaces for sentence breaking

This API implements the Unicode sentence-breaking algorithm.  It is very similar
to the one for word breaks, but without the additional parameters to support
tailoring.

```c++
namespace std::uc {
  enum class sentence_property { @*implementation defined*@ };
  sentence_property sentence_prop(uint32_t cp);

  template<utf_iter I, sentinel_for<I> S>
  I prev_sentence_break(I first, I it, S last);

  template<utf_range_like R>
  @*uc-result-iterator*@ prev_sentence_break(R&& r, @*range-like-iterator*@<R> it);

  template<utf_iter I, sentinel_for<I> S>
  I next_sentence_break(I first, S last);

  template<utf_range_like R>
  @*uc-result-iterator*@ next_sentence_break(R&& r, std::ranges::iterator_t<R> it);

  template<utf_iter I, sentinel_for<I> S>
  bool at_sentence_break(I first, I it, S last);

  template<utf_range_like R>
  bool at_sentence_break(R&& r, @*range-like-iterator*@<R> it);

  template<utf_iter I, sentinel_for<I> S>
  utf32_view<I> sentence(I first, I it, S last);

  template<utf_range_like R>
  utf32_view<ranges::iterator_t<R>> sentence(R&& r, @*range-like-iterator*@<R> it);

  struct @*sentences-t*@ : range_adaptor_closure<@*sentences-t*@> { // @*exposition only*@
    template<utf_iter I, sentinel_for<I> S>
    @*unspecified*@ operator()(I first, S last) const;

    template<utf_range_like R>
    @*unspecified*@ operator()(R&& r) const;
  };

  inline constexpr @*sentences-t*@ sentences;
}
```

These operations have the semantics previously described.

## Add interfaces for paragraph breaking

The paragraph breaking API is the same as the sentence and word breaking APIs
(again, without the extra tailoring parameters).

Unicode does not list paragraph breaks as a specific kind of text
segmentation, but it can be useful in some cases. In particular, paragraph
detection is part of the Unicode bidirectional algorithm. One way of tailoring
the behavior of the bidirectional algorithm is to process some paragraphs
separately from others; having an API for detecting paragraph breaks makes
that simpler.

Since this is a useful but unofficial kind of text segmentation, there is no
`paragraph_prop`, nor `paragraph_property`.

```c++
namespace std::uc {
  template<utf_iter I, sentinel_for<I> S>
  I prev_paragraph_break(I first, I it, S last);

  template<utf_range_like R>
  @*uc-result-iterator*@ prev_paragraph_break(R&& r, @*range-like-iterator*@<R> it);

  template<utf_iter I, sentinel_for<I> S>
  I next_paragraph_break(I first, S last);

  template<utf_range_like R>
  @*uc-result-iterator*@ next_paragraph_break(R&& r, std::ranges::iterator_t<R> it);

  template<utf_iter I, sentinel_for<I> S>
  bool at_paragraph_break(I first, I it, S last);

  template<utf_range_like R>
  bool at_paragraph_break(R&& r, @*range-like-iterator*@<R> it);

  template<utf_iter I, sentinel_for<I> S>
  utf32_view<I> paragraph(I first, I it, S last);

  template<utf_range_like R>
  utf32_view<ranges::iterator_t<R>> paragraph(R&& r, @*range-like-iterator*@<R> it);

  struct @*paragraphs-t*@ : range_adaptor_closure<@*paragraphs-t*@> { // @*exposition only*@
    template<utf_iter I, sentinel_for<I> S>
    @*unspecified*@ operator()(I first, S last) const;

    template<utf_range_like R>
    @*unspecified*@ operator()(R&& r) const;
  };

  inline constexpr @*paragraphs-t*@ paragraphs;
}
```

## Add interfaces for line breaking

The Unicode line breaking algorithm differs from the other break algorithms in
that there are multiple kinds of line breaks. Some line breaks are required,
as after a newline (e.g. `"\n"` or `"\r\n"`). These are known as *hard* line
breaks.

The line breaking algorithm produces many more line breaks, but all non-hard
line breaks are places where it is possible to break the line -- though it is
not necessary to do so. These are known as *allowed* line breaks. Higher-level
program logic must determine which of these allowed breaks is to be used, for
example to fit in available horizontal space.

The proposed interfaces only generate hard line breaks where they are
indicated in the Unicode line breaking rules and there could be an allowed
line break. Line breaks always occur at the beginning and end of any sequence,
but the API below does not report those as hard breaks; the fact that they are
hard breaks is implicit.

TODO

## Add a feature test macro

Add the feature test macro `__cpp_lib_unicode_text_segmentation`.

# Implementation experience

TODO
