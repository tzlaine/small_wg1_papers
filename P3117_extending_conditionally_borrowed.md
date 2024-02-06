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


# Notes

views containing an invocable `F`:

- `transform_view` -- add `F` to iterator
- `zip_transform_view` -- add `F` to iterator
- `adjacent_transform_view` -- add `F` to iterator


views containing a predicate `Pred`:

- `filter_view` -- add `Pred` + sentinel to iterator
- `take_while_view` -- add `Pred` to sentinel
- `chunk_by_view` -- add `Pred` + sentinel to iterator (and begin() for bidi ranges -- sentinel is not enough)

- `drop_while_view` -- already borrowed

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

`split_view` -- this one has no cached state; not sure why it's not just
borrowed if `V` and `Pattern` are.

`cartesian_product_view` -- this one actually make sense, since there's no upper
bound on `sizeof...(Vs)`.
