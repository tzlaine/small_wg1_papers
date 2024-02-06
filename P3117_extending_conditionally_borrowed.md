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
