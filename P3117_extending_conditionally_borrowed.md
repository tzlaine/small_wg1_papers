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

There is memory safety issue in this status quo, not just for these templates,
but in the general case for views that are not `borrowed_range`s.  Because the
views contain state used by their iterators, including caches, their iterators
share state and so are not regular.  If a view's `.begin()` is taken multiple
times, the operation of each iterator is not independent of the other
iterators.  It is possible to get undefined results when operating on the
iterators, even on a single thread, because some operations latch state in the
view that the iterator came from.  An operation on one iterator `i1` may latch
state in the view that another iterator `i2` then reads, when `i2` expects the
view's state to reflect only *its* (`i2`'s) sequence of operations.

This is not a memory safety issue for all views, but it is true in the general
case.  Due to the way that it does caching, `join_view` can invoke UB when
more than one iterator is in play:

```c++
auto r = /* range of subranges containing 100 elements across all the subranges */;
int count = 0;
auto joined_r = r | std::ranges::join;
for (auto && elem : joined_r) {
    ++count;
    if (count == 88) {
        // Updates the cache in joined_r to contain the first subrange in r, which is
        // probably not the current subrange in which "elem" is found.  This
        // effectively backs up the iterator used by the for loop.  The next time
        // operator++ is called on the loop's iterator, it will test its internal
        // iterator against the end of the first subrange in r; this comparison of two
        // unrelated iterators is likely to be UB.
        auto temporary = joined_r.begin();
    }
}
assert(count == 100); // Error!
```

This is an unusual bit of code to be sure.  However, nothing anywhere tells
the user that they're about to get language-level UB by writing
`joined_r.begin()`.

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

## The low-cost ones

`filter_view` could get the same treatment as the easy ones above, but its
iterator also needs to know where the end of the underlying view is, so the
sentinel would also need to be added to the iterator.  This would typically
increase the size of the iterator by the size of two pointers.

`chunk_by_view` is mostly the same story as `filter_view`.  However, for
bidirectional specializations of the view, the iterator's `operator--` would
also need the beginning of the underlying view `V`.  This would typically
increase the size of the iterator by the size of two or three pointers for
forward and bidirectional views, respectively.

## The questionable ones

`split_view` stores its view `V` and another range `Pattern` on which to do
the splitting.  If `Pattern` is a `borrowed_range` and `sizeof(Pattern)` is no
more than the size of two pointers, the pattern could be copied into the
iterator.  This would typically increase the size of the iterator by the size
of two pointers.

`lazy_split_view` is nearly the same story as `split_view`, except that it has
an additional cache:

> ```c++
>     @*non-propagating-cache*@<iterator_t<V>> current_;              // exposition only, present only
>                                                                 // if forward_range<V> is false
> ```

As you can see, this is only present if `forward_range<V>` is `false` for a
particular specialization of `lazy_split_view`.  Making the borrowed-ness
dependent on `forward_range<V>` seems reasonable, especially since
`forward_range<V>` will be `true` for vast majority of `lazy_split_view`
specializations.

There is a wrinkle for conditioning the borrowed-ness of `split_view` and
`lazy_split_view` on their `Pattern` template parameter.  The wrinkle is that
`Pattern` will frequently be `single_view`, which is never a `borrowed_range`.
This could be addressed by making `single_view` conditionally borrowed as
well, if its `T` parameter were trivially copyable and small, say `sizeof(T)
<= sizeof(void*) * 2`.

`join_view` and `join_with_view` have these caches:

> ```c++
>     non-propagating-cache<iterator_t<V>> outer_;            // exposition only, present only
>                                                             // when !forward_range<V>
>     non-propagating-cache<remove_cv_t<InnerRng>> inner_;    // exposition only, present only
>                                                             // if is_reference_v<InnerRng> is false
> ```

Like `lazy_split_view`, they could be borrowed conditionally, when
`forward_range<V>` is `true`; in that case, `inner_` could be moved into the
iterator, at a typical cost of increasing the iterator size by the size of two
pointers.

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
