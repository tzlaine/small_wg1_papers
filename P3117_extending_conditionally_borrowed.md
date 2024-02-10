---
title: "Extending Conditionally Borrowed"
document: P3117R0
date: 2024-02-05
audience:
  - SG-9
  - LEWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
  - name: Barry Revzin
    email: <barry.revzin@gmail.com>
toc: true
monofont: "DejaVu Sans Mono"

---

# Changelog

Initial revision.

# Motivation

In P2017R1, we made some range adaptors conditionally borrowed.  But we didn't
touch adaptors that had callables - like `views::transform`.  It turns out to
be very useful to have a borrowable version of `views::transform`.  Indeed,
P2728R6 even adds a dedicated new range adaptor (`views::project`) which is
simply a version of `views::transform` that can be borrowed (because its
callable must be a constant).

But rather than add a dedicated view for this specific case, which requires a
new name but really only helps `views::transform`, we can generalize
`views::transform` to address the use-case in a way that would also help all
the other range adaptors that take callables.  At the very least, in
`views::transform(r, f)` if `r` is borrowed and `f` is empty, an
implementation can simply put `f` in the `transform_view<R, F>::iterator`
directly (rather than a `transform_view<R, F> *`) which would allow it to be
borrowed.  The same could be said for other range adaptors that take callables
as well, which seems like a more useful approach as well as not requiring new
names for every adaptor.

The main question then is what the criteria should be for when
transform_view<R, F> should be a borrowed range (when R is):

- `is_empty_v<F>` (range-v3 already does this - not for conditionally borrowed,
  but just to decide whether to store the callable by value in the iterator)

- `sizeof(F) <= sizeof(void *)` and `is_trivially_copyable_v<F>` (this means
  that when transforming with a function pointer, the function pointer itself
  can live in the iterator - which takes the same amount of space as the
  parent pointer, except with one less indirection)

- something else?

This question is a little simpler for `views::transform` (which only needs to
potentially store `f` in the adapted iterator) than it is for `views::filter`
(which would need not only the predicate but also the underlying sentinel, so
this may not be worthwhile).  This would need to be carefully considered.

# Why care about `borrowed_range`?

`borrowed_range` is important because it allows the standard range adaptors
and views to know that a particular view is a proper view -- that is, a
reference type, not an expensive-to-copy type like a `std::vector<int>`.
Certain views refuse to work with non-`borrowed_range` ranges, because the
result might dangle.  For instance, you cannot construct a `subrange` from a
non-`borrowed_range`.  In other cases, an rvalue range might be wrapped in a
`owning_view` to turn it into a view, if it was not already a
`borrowed_range`.  So `std::vector<int>({1,2,3,4,5,6,7,8,9}) | take(3)` would
take the rvalue `vector`, wrap it in an `owning_view`, and the result is a
non-`borrowed_range` that is expensive to copy.

Here is the part of `[range.range]` that describes `borrowed_range`:

> ```c++
> template<class T>
>   concept borrowed_range =
>     range<T> && (is_lvalue_reference_v<T> || enable_borrowed_range<remove_cvref_t<T>>);
> ```
> [4]{.pnum} Let `U` be `remove_reference_t<T>` if `T` is an rvalue reference type, and `T`
> otherwise. Given a variable `u` of type `U`, `T` models `borrowed_range` only
> if the validity of iterators obtained from `u` is not tied to the lifetime of
> that variable.
> 
> [5]{.pnum} [_Note 2:_ Since the validity of iterators is not tied to the lifetime of a
> variable whose type models borrowed_range, a function with a parameter of such
> a type can return iterators obtained from it without danger of dangling. _? end
> note_]

To increase the interoperability of the templates in `std::ranges` and to
avoid unnecessary wrapping, we should make as many views `borrowed_range`s as
is reasonable.  It is probably unreasonable to do so if there is a significant
cost involved.  To make a view a `borrowed_range` means moving its state out
of the view itself, and into the iterator instead.

Note that moving state to the iterator is not sufficient to guarantee
borrowed-ness, though is it necessary.  Most views take an existing view `V`
and do something with it, making a new view over the existing one.  For
instance, `transform_view<V, F>` creates a view by transforming the elements
of `V` using the invocable `F`.

A particular view specialization is only borrowed if it satisfies the
`borrowed_range` concept, *and* `V` does as well.  This is important, because
if a view is not borrowed, no view that uses it will be either.  In `rng | V1
| V2 | ... | Vn`, if *any* `Vi` is not borrowed, the entire resulting view is not.

# Status quo

Currently, there are several views that are never `borrowed_range`s that
potentially could be.  They are: `transform_view`, `zip_transform_view`,
`adjacent_transform_view`, `filter_view`, `take_while_view`, `chunk_by_view`,
`join_view`, `join_with_view`, `split_view`, `lazy_split_view`, and
`cartesian_product_view`.

The status quo is also not as memory safe as it could be.  Many of the views
above have iterators that refer back to the view to get the end of the
underlying sequence `V base_`.  Consider this example.

```c++
auto v = /* some view containing 100 elements */;
int count = 0;
auto const last = std::ranges::end(v);
for (auto it = std::ranges::begin(v); it != last; ++it) {
    if (++count == 50) {
        v = /* new stuff, where end(v) is different */;
        // Some subsequent operations on 'it' will reach back into 'v' for some if
        // 'it' needs to know the end of the v.base_.  Even though we made a copy of
        // ranges::end(v) -- 'last' -- we are not protected.
    }
}
```

This is unusual code to be sure, but nothing in the standard indicates that
this is UB.  Rather than just update the wording with preconditions that
indicate the UB, we would like to copy `base_.end()` into the iterators of all
the views above.  This is a necessary step in making a view borrowable, but is
probably a good idea, independent of borrowability concerns.

## The easy ones

`join_view` is easiest of all -- it requires no modifications at all, it just
needs its borrowability predicated on its being a `forward_range`.  That's
because `join_view` and `join_with_view` have these caches:

> ```c++
>     @*non-propagating-cache*@<iterator_t<V>> outer_;            // exposition only, present only
>                                                             // when !forward_range<V>
>     @*non-propagating-cache*@<remove_cv_t<InnerRng>> inner_;    // exposition only, present only
>                                                             // if is_reference_v<InnerRng> is false
> ```

If a `join_view` uses either of its two caches, it will be an `input_range`.
So this will do the trick:

```c++
template<typename T>
  constexpr bool enable_borrowed_range<join_view<T>> =
    enable_borrowed_range<T> && forward_range<join_view<T>>;
```

Four more views are easy to change with little impact: `transform_view`,
`zip_transform_view`, `adjacent_transform_view`, and `take_while_view`.  Each
of these views stores the its invocable in the view, and has an iterator type
that contains a pointer back to the view.  However, as stated in the
Motivation section, if the invocable ("`F f`" for the first three, "`Pred
pred`" for `take_while_view`, just "`f`" hereafter) has a size no larger than
a pointer and is trivially copyable, we could just replace the iterator's (or
sentinel's, in the case of `take_while_view`) back-pointer to the view with a
copy a `f`.  This would enable borrowed-ness, and also would make these views
slightly more efficient by removing an indirection.  We could have insisted
that `F` be empty, and most `F`s that people use with these views will be.
However, since we're already replacing a pointer, we might as well allow for a
small amount of state in `F`.  Note that since the change to the
iterator/sentinel data members depends on `sizeof(F)`, the change is
statically conditional.

## The low-cost ones

`filter_view` could get the same treatment as the easy ones above (the ones
that require modification), but its iterator also needs to know where the end
of the underlying view is, so the sentinel would also need to be added to the
iterator.  This would typically increase the size of the iterator by the size
of two pointers.

`chunk_by_view` is mostly the same story as `filter_view`.  However, for
bidirectional specializations of the view, the iterator's `operator--` would
also need the beginning of the underlying view `V`.  This would typically
increase the size of the iterator by the size of two or three pointers for
forward and bidirectional views, respectively.

`split_view` and `join_with_view` each take a view `V` and a view `Pattern` to
use to do the join or split, respectively.  Clearly, to be borrowable `V` must
be borrowable.  If `Pattern` is borrowable as well, then the entire
`split_view`/`join_with_view` is too.  However, even if `Pattern` is not
borrowable, if we could copy `Pattern` into the iterator, that would work just
as well.  A `Pattern` that is trivially copyable and no larger than `sizeof(T)
<= sizeof(void*) * 2` is acceptable to copy, since a `borrowed_range`
`Pattern` will typically be about the size of two pointers.

`lazy_split_view` is nearly the same story as `split_view`, except that it has
an additional cache:

> ```c++
>     @*non-propagating-cache*@<iterator_t<V>> current_;              // exposition only, present only
>                                                                 // if forward_range<V> is false
> ```

As with `join_view`, the cache is in use if `forward_range<lazy_split_view<V,
Pattern>>` is `false`.  This means its borrowability depends on `V`,
`Pattern`, and `forward_range<lazy_split_view<V, Pattern>>`.

## The other one

`cartesian_product_view` has an unbounded number of views it may be
specialized with, so it's probably a poor candidate for moving stuff into the
iterator.

# Cost/benefit analysis

It seems like the easy ones seem like clear improvements.  The modifications
are simple, and they're really no cost.  There's even a potential performance
benefit, however meager it might be.

The low-cost ones also seems like improvements, though they come with a cost,
in terms of increased iterator size.

The rest are indeed questionable.  They will require more in-depth
modifications to the affected views.  However, the memory safety gains from
changing `join_view` and `join_with_view` may justify the effort.
