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

In P2017R1, we made some range adaptors conditionally borrowed.  But we didn’t
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
> a type can return iterators obtained from it without danger of dangling. _— end
> note_]

To increase the interoperability of the templates in `std::ranges` and to
avoid unnecessary wrapping, we should make as many views `borrowed_range`s as
is reasonable.  It is probably unreasonable to do so if there is a significant
cost invovled.  To make a view a `borrowed_range` means moving its state out
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

## The easy ones

Four of these are easy to change with little impact: `transform_view`,
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

Another one seems easy to make borrowed with no changes at all.  `split_view`
stores its view `V` and another range `Pattern` on which to do the splitting.
If we defined its `enable_borrowed_range` specialization to be
`enable_borrowed_range<V> && enable_borrowed_range<Pattern>`, it will be
borrowed when it can be.

## The low-cost ones



## The probably-nots

TODO

# Notes

views containing a predicate `Pred`:

- `filter_view` -- add `Pred` + sentinel to iterator
- `chunk_by_view` -- add `Pred` + sentinel to iterator (and begin() for bidi ranges -- sentinel is not enough)

Other things to consider:

- Why isn't `join_view` borrowed if its `V` is?  It seems to be because of the
  cached exposition-only `inner_` and `outer_` members:

```c++
    non-propagating-cache<iterator_t<V>> outer_;            // exposition only, present only
                                                            // when !forward_range<V>
    non-propagating-cache<remove_cv_t<InnerRng>> inner_;    // exposition only, present only
                                                            // if is_reference_v<InnerRng> is false
```

Those also make `join_view` problematic unless the user can guarantee that
there is at most one outstanding iterator.  Otherwise, with two or more, the
iterators will modify each other's behavior in undefined ways by stomping on
each other's shared cache in the view.

- Why aren't `join_with_view`, `split_view`, `lazy_split_view`, and
  `cartesian_product_view` borrowed when they're constructed from however many
  borrowed ranges?

`join_with_view` -- same as answer above to `join_view`.

`lazy_split_view` -- seems to be a similar reason, but less cached state:

```c++
    non-propagating-cache<iterator_t<V>> current_;              // exposition only, present only
                                                                // if forward_range<V> is false
```

`cartesian_product_view` -- this one actually make sense, since there's no upper
bound on `sizeof...(Vs)`.
