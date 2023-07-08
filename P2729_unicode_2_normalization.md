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

# Examples

# TODO

# Proposed design

## Dependencies

This proposal depends on the existence of
[P2728](https://isocpp.org/files/papers/P2728R0.html) "Unicode in the Library,
Part 1: UTF Transcoding".

## Add Unicode version observers

```c++
namespace std::uc {
  inline constexpr int major_version = @*implementation defined*@;
  inline constexpr int minor_version = @*implementation defined*@;
  inline constexpr int patch_version = @*implementation defined*@;
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

### ABI concerns

The algorithms proposed in this paper and papers to follow are often
data-driven.  (Normalization is, stream-safe is not.)  While the code of each
algorithm is going to be template code, the data will not be.  However, if the
implementation makes those data available using the right interface, the
implementation will be able to ship the same algorithm for multiple versions
of Unicode.  For example, say your favorite standard library ("StdLib")
version N contains support for normalization using Unicode X.Y.Z.  Say also
that its NFC data table (containing the data that drive NFC normalization)
comes from a function that looks like this:

```c++
// In <unicode> or similar.

namespace std::uc::__detail {
    // Note that this is a non-template.
    struct __normalization_table_data {
        // members ...
    };

    constexpr __normalization_table_data const & __get_nfc_table();
    
    template</* ... */>
    constexpr auto __norm_impl(/* ... */) {
        __normalization_table_data const & table = __get_nfc_table();
        // Do normalization ...
    }
}
```

Now say that version N+1 of StdLib is release, supporting Unicode (X+2).Y.Z.
Since the normalization code must match the data that drive it, now we have
broken ABI compatibility between builds of StdLib N and StdLib N+1.  We cannot
provide a new data table without one version of the template code being out of
sync with it.

Instead, StdLib could do this:

```c++
// In <unicode> or similar.

namespace std::uc::__detail {
    // Note that this is a non-template.
    struct __normalization_table_data {
        // members ...
    };

    template<int _Major, int _Minor, int _Patch>
    constexpr __normalization_table_data __get_nfc_table();

    template<>
    constexpr __normalization_table_data __get_nfc_table<major_version, minor_version, patch_version>();
    
    template</* ... */>
    constexpr auto __norm_impl(/* ... */) {
        __normalization_table_data const & table =
            __get_nfc_table<major_version, minor_version, patch_version>();
        // Do normalization ...
    }
}
```

This effectively ABI-tags each data table with the major+minor+patch version
of Unicode that it belongs with.  Though Boost.Text deals only with one
version of Unicode at a time, the non-version-specific code above is
structurally very similar to the implementation in Boost.Text, so I know this
approach is workable.

## Add stream-safe view and adaptor

```c++
namespace std:: uc {
  constexpr int @*stream-safe-max-nonstarters*@ = @*implementation defined*@;

  template<class T>
  constexpr bool @*is-utf-iter*@ = false;                                 // @*exposition only*@
  template<format F, format T, class I, class S, class E>
  constexpr bool @*is-utf-iter*@<utf_iterator<F, T, I, S, E>> = true;     // @*exposition only*@

  template<class V>
  constexpr auto @*uc-view-category*@() {                                 // @*exposition only*@
    if constexpr (ranges::common_range<V> && ranges::bidirectional_range<V>) {
      return bidirectional_iterator_tag{};
    } else {
      return forward_iterator_tag{};
    }
  }
  template<class V>
  using @*uc-view-category-t*@ = decltype(uc_view_category<V>());         // @*exposition only*@

  constexpr int @*uc-ccc*@(char32_t cp);                                  // @*exposition only*@

  template<typename I, typename S>
    I @*next-stream-safe-cp*@(I first, S last, int & nonstarters) {
      for (; first != last; ++first) {
        char32_t const cp = *first;
        if (@*uc-ccc*@(cp) == 0)
          nonstarters = 0;
        else
          ++nonstarters;
        if (nonstarters <= @*stream-safe-max-nonstarters*@)
          break;
      }
      return first;
    }

  template<utf32_range V>
    requires ranges::view<V> && ranges::forward_range<V>
  class stream_safe_view : public ranges::view_interface<stream_safe_view<V>>
  {
    template<bool Const, bool StoreLast = !@*is-utf-iter*@<ranges::iterator_t<V>>>
    class @*iterator*@;                                                   // @*exposition only*@
    class @*sentinel*@;                                                   // @*exposition only*@

    static constexpr bool @*bidi*@ =                                      // @*exposition only*@
      derived_from<@*uc-view-category-t*@<V>, bidirectional_iterator_tag>;

    V @*base_*@ = V();                                                    // @*exposition only*@

  public:
    constexpr stream_safe_view() requires default_initializable<V> = default;
    constexpr stream_safe_view(V base) : @*base_*@{std::move(base)} {}

    constexpr V base() const & requires copy_constructible<V> { return @*base_*@; }
    constexpr V base() && { return std::move(@*base_*@); }

    constexpr @*iterator*@<false> begin() { return @*iterator*@<false>{@*base_*@}; }
    constexpr @*iterator*@<true> begin() const requires utf32_range<const V> { return @*iterator*@<true>{@*base_*@}; }

    constexpr @*sentinel*@ end() { return @*sentinel*@{}; }
    constexpr @*iterator*@<false> end() requires @*bidi*@ { return @*iterator*@<false>{base_, ranges::end(base_)}; }
    constexpr @*sentinel*@ end() const requires utf32_range<const V> { return @*sentinel*@{}; }
    constexpr @*iterator*@<true> end() const requires utf32_range<const V> && @*bidi*@
      { return @*iterator*@<true>{base_, ranges::end(base_)}; }
  };

  template<utf32_range V>
    requires ranges::view<V> && ranges::forward_range<V>
  template<bool Const, bool StoreLast>
  class stream_safe_view<V>::@*iterator*@
    : public std::iterator_interface<
        @*iterator*@<Const, StoreLast>,
        @*uc-view-category-t*@<V>,
        char32_t,
        char32_t>
  {
    using @*Base*@ = @*maybe-const*@<Const, V>;                               // @*exposition only*@

    static constexpr bool @*bidi*@ =                                      // @*exposition only*@
      derived_from<@*uc-view-category-t*@<V>, bidirectional_iterator_tag>;

    ranges::iterator_t<@*Base*@> @*it_*@;                                     // @*exposition only*@
    ranges::iterator_t<@*Base*@> @*first_*@;                                  // @*exposition only*@
    ranges::sentinel_t<@*Base*@> @*last_*@;                                   // @*exposition only*@
    int @*nonstarters_*@ = 0;                                             // @*exposition only*@

    constexpr auto @*begin_iter*@(ranges::iterator_t<@*Base*@> i) const;  // @*exposition only*@
    constexpr auto @*end_iter*@(ranges::iterator_t<@*Base*@> i) const;    // @*exposition only*@

    friend class @*sentinel*@;

  public:
    constexpr @*iterator*@() requires default_initializable<ranges::iterator_t<@*Base*@>> = default;
    constexpr @*iterator*@(@*Base*@ & base) : @*it_*@(ranges::begin(base)) {
      if (@*it_*@ != @*end_iter*@(@*it_*@) && @*uc-ccc*@(*@*it_*@))
        @*nonstarters_*@ = 1;
    }
    constexpr @*iterator*@(@*Base*@ & base, ranges::iterator_t<V> it) requires @*bidi*@
      : @*it_*@(it) {}
    constexpr @*iterator*@(@*iterator*@<!Const, StoreLast> i)
      requires Const && convertible_to<ranges::iterator_t<V>, ranges::iterator_t<@*Base*@>>
      : @*it_*@(i.@*it_*@), @*nonstarters_*@(i.@*nonstarters_*@) {}

    constexpr const ranges::iterator_t<@*Base*@> & base() const & noexcept { return @*it_*@; }
    constexpr ranges::iterator_t<@*Base*@> base() && { return std::move(@*it_*@); }

    constexpr char32_t operator*() const { return *@*it_*@; }

    constexpr @*iterator*@ & operator++() {
      if (it_ == last_)
        return *this;
      ++it_;
      it_ = @*next-stream-safe-cp*@(it_, last_, nonstarters_);
      return *this;
    }
    constexpr @*iterator*@ & operator--() requires @*bidi*@;

    friend bool operator==(@*iterator*@ lhs, @*iterator*@ rhs)
      { return lhs.base() == rhs.base(); }

    using @*base-type*@ = std::iterator_interface<                        // @*exposition only*@
      @*iterator*@<Const, StoreLast>,
      @*uc-view-category-t*@<V>,
      char32_t,
      char32_t>;
    using @*base-type*@::operator++;
    using @*base-type*@::operator--;
  };

  template<utf32_range V>
    requires ranges::view<V> && ranges::forward_range<V>
  class stream_safe_view<V>::@*sentinel*@
  {
  public:
    template<bool Const, bool StoreLast>
    friend constexpr bool operator==(const @*iterator*@<Const, StoreLast> & it, @*sentinel*@) {
      if constexpr (StoreLast) {
        return it.@*it_*@ == it.last_;
      } else {
        return it.base().base() == it.base().end();
      }
    }
  };

  template<class R>
  stream_safe_view(R &&) -> stream_safe_view<views::all_t<R>>;
}

namespace std::ranges {
  template<class V>
  inline constexpr bool enable_borrowed_range<uc::stream_safe_view<V>> = enable_borrowed_range<V>;
}

namespace std::uc {
  inline constexpr @*unspecified*@ as_stream_safe;
}
```

The exposition-only function `@*uc-ccc*@()` returns the [Canonical Combining
Class](https://unicode.org/reports/tr44/#Canonical_Combining_Class_Values),
which indicates how and whether a code point combines with other code points.
For some code point `cp`, `@*uc-ccc*@(cp) == 0` iff `cp` is a
"starter"/"noncombiner".  Any number of "nonstarters"/"combiners" may follow a
starter (remember that the purpose of the stream-safe format is to limit the
maximum number of combiners to at most 30).

This design truncates sequences of nonstartes longer than
`@*stream-safe-max-nonstarters*@`, meaning that it skips over the subsequent
nonstarters.  The Unicode standard shows a technique for inserting special
dummy-starters (that do not interact with most other text) every 30
non-starters, so that the original input is preserved.  I think this is silly
-- the longest possible meaningful sequence of nonstarters is 18 code points,
and that is only necessary for backwards compatibility.  This means a value of
18 for `@*stream-safe-max-nonstarters*@` is fine.  Typcial sequences are
*much* shorter.

```c++
constexpr @*iterator*@ & operator--() requires @*bidi*@;
```

Effects:

- If `@*it_*@ == @**@` is `true`, no effect.
- Otherwise, if `@*it_*@ > @*nonstarters_*@`, decrements `@*it_*@` and
  `@*nonstarters_*@`.
- Otherwise, as if by:
```c++
    auto const initial_it = @*it_*@;
    auto it = ranges::find_last_if(@*begin_iter*@(), @*it_*@, [](auto cp) { return @*uc-ccc*@(cp) == 0; });
    auto const from = it == @*it_*@ ? @*begin_iter*@(): ranges::next(it);
    ptrdiff_t const nonstarters = distance(from, @*it_*@);
    @*nonstarters_*@ = std::min(nonstarters, ptrdiff_t(@*stream-safe-max-nonstarters*@ - 1));
    if (@*nonstarters_*@)
      @*it_*@ = ranges::next(from, @*nonstarters_*@);
    if (@*it_*@ == initial_it)
      --@*it_*@;
```

Returns: `*this`.

### Why `stream_safe_view` is forward-or-better

When reading forward to find the next valid code point, each code point is
read, using `*@*it_*@`.  This consumes the value in an input range, so the
subsequent call to `stream_safe_view::iterator::operator*()` would not read
the value found, but the next one instead.

### `stream_safe_view::iterator`

The exposition-only data member `first_` is defined if and only if
`!@*is-utf-iter*@<ranges::iterator_t<V>> && ranges::common_range<V> &&
ranges::bidirectional_range<V>` is `true`.  The exposition-only data member
`first_` is defined if and only if `!@*is-utf-iter*@<ranges::iterator_t<V>>` is
`true`.

The exposition-only function `@*begin_iter*@` returns `first_` if there is a
`first_` data member.  Otherwise, if `@*is-utf-iter*@<ranges::iterator_t<V>>`
is `true`, it returns `std::ranges::iterator_t<V>(i.begin(), i.begin(),
i.end())`.  Otherwise, it returns `void`.

The exposition-only function `@*end_iter*@` returns `last_` if there is a
`last_` data member.  Otherwise, it returns
`std::ranges::iterator_t<V>(i.begin(), i.end(), i.end())` if that is well
formed.  Otherwise, it returns `i.end()`.

### Why `first_` and `last_` are conditionally defined

For some specializations of `stream_safe_view`, the iterator has all the
information necessary for the `stream_safe_view` function.  Specifically, this
is the case for a `stream_safe_view` adapting a view whose iterator type is a
specialization of `utf_iter`.  `utf_iter` contains the current posisiton, the
end of the range it is adapting, and often even the beginning of the range it
is adapting.

To prevent carrying around a sentinel value that reproduces the data in the
iterator value, `first_` and `last_` can be elided.

## Stream-safe adaptor

The name `as_stream_safe` denotes a range adaptor object
([range.adaptor.object]).  Let `E` be an expression and let `T` be
`remove_cvref_t<decltype((E))>`.  If `as_utf32(decltype((E)))` is not
well-formed, `as_stream_safe(E)` is ill-formed.  Additionally, if
`decltype((E))` does not model `forward_range` and `is_pointer_v<T>` is not
`true`, `as_stream_safe(E)` is ill-formed.  The expression `as_stream_safe(E)`
is expression-equivalent to:

- If `T` is a specialization of `empty_view` ([range.empty.view]), then
  `@*decay-copy*@(E)`.
- Otherwise, if `T` is a specialization of `stream_safe_view`, then `E`.
- Otherwise, if `T` is a specialization of `utf32_view`, then
  `stream_safe_view(E)`.
- Otherwise, `stream_safe_view(as_utf32(E))`.

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
