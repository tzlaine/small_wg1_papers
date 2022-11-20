---
title: "`std::iterator_interface`"
document: D0000R0
date: 2022-11-18
audience:
  - LEWG-I
  - LEWG
author:
  - name: Zach Laine
    email: <whatwasthataddress@gmail.com>
toc: false
monofont: "DejaVu Sans Mono"

---

# Motivation

## The story for everyday users writing STL iterators is not great

Writing STL iterators is surprisingly hard.  There are a lot of things that
can subtly go wrong.  It is also very tedious, which of course makes it
error-prone.

Iterators have numerous typedefs and operations, even though all the
operations of a given iterator can be implemented in terms of at most four
operations (and usually only three).  Writing all the other operations yields
very similar-looking code that is hard to review, and all but requires that
you write full-coverage tests for each iterator.

As an example, say you wanted an iterator that allowed you to do iterate over
repetitions of a sequence of characters, like:

```cpp
repeated_chars_iterator first("foo", 3, 0); // 3 is the length of "foo", 0 is this iterator's position.
repeated_chars_iterator last("foo", 3, 7);  // Same as above, but now the iterator's position is 7.
std::string result;
std::copy(first, last, std::back_inserter(result));
assert(result == "foofoof");
```

Here's how you might implement it, with and without this proposal:

::: tonytable

### Before
```c++
struct repeated_chars_iterator
{
    using value_type = char;
    using difference_type = std::ptrdiff_t;
    using pointer = char const *;
    using reference = char const;
    using iterator_category =
        std::random_access_iterator_tag;

    constexpr repeated_chars_iterator() :
        first_(nullptr),
        size_(0),
        n_(0)
    {}
    constexpr repeated_chars_iterator(
        char const * first,
        difference_type size,
        difference_type n) :
        first_(first),
        size_(size),
        n_(n)
    {}

    constexpr reference operator*() const
    {
        return first_[n_ % size_];
    }

    constexpr value_type operator[](
        difference_type n) const
    {
        return first_[(n_ + n) % size_];
    }

    constexpr repeated_chars_iterator & operator++()
    {
        ++n_;
        return *this;
    }
    constexpr repeated_chars_iterator
    operator++(int)noexcept
    {
        repeated_chars_iterator retval = *this;
        ++*this;
        return retval;
    }
    constexpr repeated_chars_iterator &
    operator+=(difference_type n)
    {
        n_ += n;
        return *this;
    }

    constexpr repeated_chars_iterator & operator--()
    {
        --n_;
        return *this;
    }
    constexpr repeated_chars_iterator
    operator--(int)noexcept
    {
        repeated_chars_iterator retval = *this;
        --*this;
        return retval;
    }
    constexpr repeated_chars_iterator &
    operator-=(difference_type n)
    {
        n_ -= n;
        return *this;
    }

    friend constexpr bool operator==(
        repeated_chars_iterator lhs,
        repeated_chars_iterator rhs)
    {
        return lhs.first_ == rhs.first_ &&
            lhs.n_ == rhs.n_;
    }
    friend constexpr bool operator!=(
        repeated_chars_iterator lhs,
        repeated_chars_iterator rhs)
    {
        return !(lhs == rhs);
    }
    friend constexpr bool operator<(
        repeated_chars_iterator lhs,
        repeated_chars_iterator rhs)
    {
        return lhs.first_ == rhs.first_ &&
            lhs.n_ < rhs.n_;
    }
    friend constexpr bool operator<=(
        repeated_chars_iterator lhs,
        repeated_chars_iterator rhs)
    {
        return lhs == rhs || lhs < rhs;
    }
    friend constexpr bool operator>(
        repeated_chars_iterator lhs,
        repeated_chars_iterator rhs)
    {
        return rhs < lhs;
    }
    friend constexpr bool operator>=(
        repeated_chars_iterator lhs,
        repeated_chars_iterator rhs)
    {
        return lhs <= rhs;
    }

    friend constexpr repeated_chars_iterator
    operator+(repeated_chars_iterator lhs,
              difference_type rhs)
    {
        return lhs += rhs;
    }
    friend constexpr repeated_chars_iterator
    operator+(difference_type lhs,
              repeated_chars_iterator rhs)
    {
        return rhs += lhs;
    }
    friend constexpr repeated_chars_iterator
    operator-(repeated_chars_iterator lhs,
              difference_type rhs)
    {
        return lhs -= rhs;
    }
    friend constexpr difference_type operator-(
        repeated_chars_iterator lhs,
        repeated_chars_iterator rhs)
    {
        return lhs.n_ - rhs.n_;
    }

private:
    char const * first_;
    difference_type size_;
    difference_type n_;
};
```

### After
```c++
struct repeated_chars_iterator :
    std::iterator_interface<
        repeated_chars_iterator,
        std::random_access_iterator_tag,
        char,
        char>
{
    constexpr repeated_chars_iterator() :
        first_(nullptr),
        size_(0),
        n_(0)
    {}
    constexpr repeated_chars_iterator(char const * first,
                                      difference_type size,
                                      difference_type n) :
        first_(first),
        size_(size),
        n_(n)
    {}

    constexpr char operator*() const
    {
        return first_[n_ % size_];
    }
    constexpr repeated_chars_iterator &
    operator+=(std::ptrdiff_t i)
    {
        n_ += i;
        return *this;
    }
    constexpr auto operator-(
        repeated_chars_iterator other) const
    {
        return n_ - other.n_;
    }

private:
    char const * first_;
    difference_type size_;
    difference_type n_;
};
```

:::

I used that motivating example in the README file for the Boost library this
proposal is based on.  One day, someone pointed out that there was a bug in
the "before" version of the iterator.  The iterator was used extensively in
yet another proposed Boost library of mine, and yet this bug was never
actually exercised that I know of.  This underscores an important point: with
very large APIs like the std iterators, one is very likely to use lots of copy
pasta, and one is very unlikely to fully test each element of the API.

I've reintroduced the bug here for funsies; see how long it takes you to find
it by inspection.

## Writing STL iterators is very useful

Some examples of useful iterators you may want to write:

- Checking iterators that do certain checks only in debug builds.
- Iterators that adapt legacy container-like types for use with STL algorithms
  and containers.
- *Ad hoc*, single-application iterators.

Because writing iterators is so much work at the moment, most of use avoid it
whenever possible.  So *ad hoc* use cases for iterators almost always go
unfulfilled.  Here's an example of one such *ad hoc* iterator use case.

Say you have two or more sequences that you want to treat as a single sequence:

```cpp
std::vector<int> a;
std::list<int> b;
```

You'd use something like the `views::concat` proposed in [@P2542R2]:

```cpp
for (auto && x : concat(a, b)) {
    // ...
}
```

That works well, as long as there exists a common reference type among the
iterators used within the `views::concat` implementation.  What if there
isn't?  What if the `value_type`s of the various ranges you pass to `concat()`
are essentially compatible, but nevertheless have no common reference type?

For example, what if I want to concatenate two ranges, one of which is a range of `int` and the other of which is a range of:

```cpp
struct my_int
{
    my_int() = default;
    explicit my_int(int v) : value_{v} {}
    int value_;
};
```

Clearly, `std::common_reference_with<my_int, int>` is `false`.  However, if I
also had some simply way of projecting from either one to `int`, like this:

```cpp
int as_int(int i) { return i; }
int as_int(my_int i) { return i.value_; }
```

I would still expect to be able to construct some view over the concatenation
of `a` and `b` -- even if it were read-only.

This example may seem contrived, but it is not.  While implementing an
algorithm to support Unicode operations on strings, I ran in to a case in
which a certain step `S` of the algorithm required iterating over two
containers `A a` and `B b`, where `A::iterator::reference` and
`B::iterator::reference` were different enough that there was no common
reference, but `A::value_type` and `B::value_type` were similar enough that I
could just call a function (analogous to `as_int()` above) that could project
both of them to a single type.  To make matters worse, I could not even break
`S` up into the `a`-part and the `b`-part, because `S` could not consier
elements in isolation; it required examining adjacent elements in the
concatenation of `a` and `b`.

None of the views proposed for the standard has ever supported projections.  I
don't consider this a defect; it's probably the wrong kind of customization
for a general-purpose view.  So, what to do?  Write an iterator that fits this
exact, one-off special case, of course.  Using
[Boost.STLInterfaces](https://github.com/boostorg/stl_interfaces), in
implementation of everything in this proposal and more, I wrote something like
this iterator.  To keep all the Unicode-y bits out of our way, I'm showing the
`int`/`my_int` analogous solution.

```cpp
template<typename T, typename Proj, typename R1, typename R2>
struct concat_iter : boost::stl_interfaces::proxy_iterator_interface<
                         concat_iter<T, Proj, R1, R2>,
                         std::bidirectional_iterator_tag,
                         T>
{
    using first_iterator = decltype(std::declval<R1 &>().begin());
    using second_iterator = decltype(std::declval<R2 &>().begin());
    struct end_tag{};

    concat_iter() : in_r1_(false) {}
    concat_iter(first_iterator it,
                first_iterator r1_last,
                second_iterator r2_first,
                Proj proj) :
        r1_last_(r1_last),
        it1_(it),
        r2_first_(r2_first),
        it2_(r2_first),
        proj_(proj),
        in_r1_(true)
    {
        if (it1_ == r1_last_)
            in_r1_ = false;
    }
    concat_iter(second_iterator it,
                first_iterator r1_last,
                second_iterator r2_first,
                Proj proj,
                end_tag) :
        r1_last_(r1_last),
        it1_(r1_last),
        r2_first_(r2_first),
        it2_(it),
        proj_(proj),
        in_r1_(false)
    {}

    concat_iter & operator++() noexcept
    {
        if (in_r1_) {
            assert(it1_ != r1_last_);
            ++it1_;
            if (it1_ == r1_last_)
                in_r1_ = false;
        } else {
            ++it2_;
        }
        return *this;
    }

    concat_iter & operator--() noexcept
    {
        if (!in_r1_) {
            if (it2_ == r2_first_) {
                in_r1_ = true;
                --it1_;
            } else {
                --it2_;
            }
        } else {
            --it1_;
        }
        return *this;
    }

    T operator*() const noexcept
    {
        return in_r1_ ? proj_(*it1_) : proj_(*it2_);
    }

    friend bool operator==(concat_iter lhs, concat_iter rhs)
    {
        return lhs.in_r1_ == rhs.in_r1_ &&
               (lhs.in_r1_ ?
                lhs.it1_ == rhs.it1_ : lhs.it2_ == rhs.it2_);
    }

    using base_type =
        boost::stl_interfaces::proxy_iterator_interface<
            concat_iter<T, Proj, R1, R2>,
            std::bidirectional_iterator_tag,
            T>;
    using base_type::operator++;
    using base_type::operator--;

private:
    first_iterator r1_last_;
    first_iterator it1_;
    second_iterator r2_first_;
    second_iterator it2_;
    Proj proj_;
    bool in_r1_;
};
```

That's a pleasantly low amount of code to write, and I was able to avoid
implementing most of the boilerplate bits -- the operations that need to exist
for my type to be conforming bidirectional iterator, but I which I will not
explicitly use in my use case.

In case you were wondering about the `T` template parameter, that just gets
around the need for some potentially-messy metaprogramming to determine what
`concat_iter`'s `value_type` is -- it's whatever you specify for `T`.

With the iterator in hand, it becomes trivial to construct a `concat_view`,
and a `concat()` function:

```cpp
template<typename T, typename R1, typename R2>
struct concat_view
    : std::view_interface<concat_view<T, R1, R2>>
{
    using iterator = concat_iter<T, R1, R2>;

    concat_view(iterator first, iterator last) :
        first_(first), last_(last)
    {}

    iterator begin() const noexcept { return first_; }
    iterator end() const noexcept { return last_; }

private:
    iterator first_;
    iterator last_;
};

template<typename T, typename R1, typename R2, typename Proj>
auto concat(R1 & r1, R2 & r2, Proj proj)
{
    using iterator = concat_iter<T, Proj, R1, R2>;
    return concat_view<T, R1, R2>(
        iterator(r1.begin(), r1.end(), r2.begin(), proj),
        iterator(r2.end(), r1.end(), r2.begin(), proj,
                 iterator::end_tag{}));
}
```

Finally, here it is at the point of use:

```cpp
std::vector<int> vec = { 0, 58, 48 };
std::list<my_int> list = { 80, 39 };
auto const v = concat<int>(vec, list, [](auto x) { return as_int(x); });
assert(std::ranges::find(v, 42) == v.end());
assert(std::ranges::find(v, 58) == std::next(v.begin()));
```

Some things to note here:

- The amount of boilerplate is very small.
- The total amount of code is very small.
- This abstraction allows me to concentrate on the algorithm I want to do on
  the concatenated sequence, rather than the details of dealing with boundary
  conditions between the underlying sequences, *in addition to* the algorithm
  itself.  Imagine if I had to have logic like that found in
  `concat_iter::operator++()` *intermixed* with the algorithm code.

When opportunities like this come up -- that is, opportunities to write custom
iterators -- the dominant consideration is what it costs to write an iterator.
If the cost is too high, we don't write one, and the algorithm that we would
have written in terms of the unwritten iterator becomes much more complicated
(and thus error-prone).  If the cost is low, we will start to notice
opportunities to write custom iterators in many more places.  One thing in
particular that easy-to-write iterators enable is easy-to-write views and view
adaptors.

## Adaptation

Sometimes, you want to take an existing iterator and adapt it for a different
use.  Since you already have multiple of the iterator operations defined on
it, it would be useful to simply use those, and fill in the missing ones, or
perhaps replace the ones that you want to work differently.

For example, if we wanted to make a filtering iterator for something like
`filter_view`, we could write it like this:

```cpp
template<typename Pred>
struct filtered_int_iterator : boost::stl_interfaces::iterator_interface<
                                   filtered_int_iterator<Pred>,
                                   std::forward_iterator_tag,
                                   int>
{
    filtered_int_iterator() : it_(nullptr) {}
    filtered_int_iterator(int * it, int * last, Pred pred) :
        it_(it),
        last_(last),
        pred_(std::move(pred))
    { it_ = std::find_if(it_, last_, pred_); }

    // A forward iterator based on iterator_interface usually required
    // three user-defined operations.  since we are adapting an existing
    // iterator (an int *), we only need to define this one.  The others are
    // implemented by iterator_interface, using the underlying int *.
    filtered_int_iterator & operator++()
    {
        it_ = std::find_if(std::next(it_), last_, pred_);
        return *this;
    }

    // It is really common for iterator adaptors to have a base() member
    // function that returns the adapted iterator.
    int * base() const { return it_; }

private:
    // Provide access to these private members.
    friend boost::stl_interfaces::access;

    // These functions are picked up by iterator_interface, and used to
    // implement any operations that you don't define above.  They're not
    // called base() so that they do not collide with the base() member above.
    constexpr decltype(auto) base_reference(this auto& self) noexcept
    { return self.it_; }

    int * it_;
    int * last_;
    Pred pred_;
};
```

Though this implementation is limited to `int*`, it's pretty easy to see that
you could change it to take an `Iterator` template parameter instead, and
after replacing `int*` with `Iterator` throughout, everything would "just
work".

# Proposed design

The proposal is to add a CRTP base template that will ease writing iterators,
in the same way that `std::view_interface` eases the writing of views today.

Before looking at the main `iterator_interface` template, we neeed to look at
some of the bits that allow that template to function.  First,
`iterator_interface_access`.  `iterator_interface_access` is used by the
adaptation logic to get access to the underlying iterator being adapted, which
is usually private.  The user just needs to friend `iterator_interface_access`
from the iterator they write (just like the friend declaration in the
`filtered_int_iterator` example above).

```cpp
struct iterator_interface_access
{
  template<typename D>
  static constexpr auto base(D& d) noexcept
    -> decltype(d.base_reference())
  {
    return d.base_reference();
  }
  template<typename D>
  static constexpr auto base(const D& d) noexcept
    -> decltype(d.base_reference())
  {
    return d.base_reference();
  }
};
```

Next, `proxy_arrow_result`, a specialization of which is used as the default
`pointer` for proxy iterators created using the `proxy_iterator_interface`
alias template (shown a bit later).

```cpp
template<typename T>
  requires is_object_v<T>
struct proxy_arrow_result
{
  constexpr proxy_arrow_result(const T& value) noexcept(
    noexcept(T(value))) :
    value_(value)
  {}
  constexpr proxy_arrow_result(T&& value) noexcept(
    noexcept(T(std::move(value)))) :
    value_(std::move(value))
  {}

  constexpr const T* operator->() const noexcept { return &value_; }
  constexpr T* operator->() noexcept { return &value_; }

private:
  T value_;
};
```

Next, a couple of exposition-only functions.  These functions make the
formation of a pointer to return from `operator->()` uniform, whether the
iterator is a proxy (returning `proxy_arrow_result` or a similar type) or not.

```cpp
template<typename Pointer, typename T>
  requires is_pointer_v<Pointer>
    decltype(auto) @*make-iterator-pointer*@(T&& value) // @*exposition only*@
      { return std::addressof(value); }

template<typename Pointer, typename T>
  auto @*make-iterator-pointer*@(T&& value)             // @*exposition only*@
    { return Pointer(std::forward<T>(value)); }
```

Finally, `iterator_interface` itself:

```cpp
// @*exposition only*@
template<typename D, typename DifferenceType>
  concept @*has-plus-eq*@ = requires (D d) { d += DifferenceType(1); };

template<
  typename D,
  typename IteratorConcept,
  typename ValueType,
  typename Reference = ValueType&,
  typename Pointer = ValueType*,
  typename DifferenceType = ptrdiff_t>
  requires is_class_v<D> && same_as<D, remove_cv_t<D>>
class iterator_interface
{
private:
  constexpr D& derived() noexcept {              // @*exposition only*@
    return static_cast<D&>(*this);
  }
  constexpr const D& derived() const noexcept {  // @*exposition only*@
    return static_cast<const D&>(*this);
  }

public:
  using iterator_concept = IteratorConcept;
  using iterator_category = iterator_concept;
  using value_type = remove_const_t<ValueType>;
  using reference = Reference;
  using pointer = conditional_t<
    is_same_v<iterator_concept, output_iterator_tag>>,
    void,
    Pointer>;
  using difference_type = DifferenceType;

  constexpr decltype(auto) operator*()
    requires requires (D d) { *iterator_interface_access::base(d); } {
      return *iterator_interface_access::base(derived());
    }
  constexpr decltype(auto) operator*() const
    requires requires (const D d) { *iterator_interface_access::base(d); } {
      return *iterator_interface_access::base(derived());
    }

  constexpr auto operator->()
    requires requires (D d) { *d; } {
      return @*make-iterator-pointer*@<pointer>(*derived());
    }
  constexpr auto operator->() const
    requires requires (const D d) { *d; } {
      return @*make-iterator-pointer*@<pointer>(*derived());
    }

  constexpr decltype(auto) operator[](difference_type n) const
    requires requires (const D d) { d + n; } {
    D retval = derived();
    retval += n;
    return *retval;
  }

  constexpr decltype(auto) operator++()
    requires requires (D d) { ++iterator_interface_access::base(d); } &&
      (!@*has-plus-eq*@<D, difference_type>) {
        ++iterator_interface_access::base(derived());
        return derived();
      }
  constexpr decltype(auto) operator++()
    requires requires (D d) { d += difference_type(1); } {
      return derived() += difference_type(1);
    }
  constexpr auto operator++(int) requires requires (D d) { ++d; } {
    D retval = derived();
    ++derived();
    return retval;
  }
  constexpr decltype(auto) operator+=(difference_type n)
    requires requires (D d) { iterator_interface_access::base(d) += n; } {
      iterator_interface_access::base(derived()) += n;
      return derived();
    }
  friend constexpr auto operator+(D it, difference_type n)
    requires requires { it += n; } {
      return it += n;
    }
  friend constexpr auto operator+(difference_type n, D it)
    requires requires { it += n; } {
      return it += n;
    }

  constexpr decltype(auto) operator--()
    requires requires (D d) { --iterator_interface_access::base(d); } &&
      (!@*has-plus-eq*@<D, difference_type>) {
        --iterator_interface_access::base(derived());
        return derived();
      }
  constexpr decltype(auto) operator--()
    requires requires (D d) { d += -difference_type(1); } {
      return derived() += -difference_type(1);
    }
  constexpr auto operator--(int) requires requires (D d) { --d; } {
    D retval = derived();
    --derived();
    return retval;
  }
  constexpr decltype(auto) operator-=(difference_type n)
    requires requires (D d) { d += -n; } {
      return derived() += -n;
    }
  friend constexpr auto operator-(D lhs, D rhs)
    requires requires { iterator_interface_access::base(lhs) -
                        iterator_interface_access::base(rhs); } {
      return iterator_interface_access::base(lhs) -
             iterator_interface_access::base(rhs);
    }
  friend constexpr auto operator-(D it, difference_type n)
    requires requires { it += -n; } {
      return it += -n;
    }

  friend constexpr bool operator<(D lhs, D rhs)
    requires equality_comparable<D> {
      return (lhs - rhs) < typename D::difference_type(0);
    }
  friend constexpr bool operator<=(D lhs, D rhs)
    requires equality_comparable<D> {
      return (lhs - rhs) <= typename D::difference_type(0);
    }
  friend constexpr bool operator>(D lhs, D rhs)
    requires equality_comparable<D> {
      return (lhs - rhs) > typename D::difference_type(0);
    }
  friend constexpr bool operator>=(D lhs, D rhs)
    requires equality_comparable<D> {
      return (lhs - rhs) >= typename D::difference_type(0);
    }
};
```

Additionally, we want free a `operator==` (and compiler-provided
`operator!=`), shown here.  In addition to the constraints shown, they require
that `D1` and `D2` are derived from specializations of `iterator_interface`.
This overload picks up comparisons of iterators of different, but
interoperable types (like `std::vector<int>::const_iterator` and
`std::vector<int>::iterator`).  As long as one iterator is convertible to the
other (in either direction), and either: 1) their adapted bases are
comparable; or 2) a `D1` is subtractable from a `D1`.

I know those contraints look oddly specific; here's why they are the way they
are:

Every category of iterator besides output need to define `operator==`.  For
this reason, each user-defined iterator derived from a specialization of
`iterator_interface` will define its own `operator==`, unless it is an output
iterator, *or* unless it is a random access (or contiguous) iterator.  The
exception for random access/contiguous is because `operator-`, which the user
must define for a random access/contiguous iterator, can be used to implement
`operator==` -- for two iterators i1 and i2, just check if `i1 - i2 == 0`.
This explains part of the constraints; this free overload is the
implementation for `operator==` for random access/contiguous iterators.

For the base-equality-comparable part, similar logic applies: we don't want
the user to have to define `operator==` when the underlying adapted iterator
undoubtedly already has it (unless it's an output iterator, of course).  This
free function supplies it in that case as well.

```cpp
template<typename D1, typename D2>
  concept @*base-iter-comparable*@ =              // @*exposition only*@
    requires (D1 d1, D2 d2) {
      iterator_interface_access::base(d1) ==
      iterator_interface_access::base(d2);
    };

template<typename D1, typename D2>
  constexpr bool operator==(D1 lhs, D2 rhs)
    requires (is_convertible_v<D2, D1> || is_convertible_v<D1, D2>) &&
             (@*base-iter-comparable*@<D1, D2> ||
              requires (D1 d) { d - d; }) {
      if constexpr (@*base-iter-comparable*@<D1, D2>) {
        return (iterator_interface_access::base(lhs) ==
                iterator_interface_access::base(rhs));
      } else if constexpr (requires (D1 d) { d - d; }) {
        return (lhs - rhs) == typename D1::difference_type(0);
      }
    }
```

Finally, there's an alias that makes it easier to define proxy iterators:

```cpp
template<
    typename Derived,
    typename IteratorConcept,
    typename ValueType,
    typename Reference = ValueType,
    typename DifferenceType = ptrdiff_t>
using proxy_iterator_interface = iterator_interface<
    Derived,
    IteratorConcept,
    ValueType,
    Reference,
    proxy_arrow_result<Reference>,
    DifferenceType>;
```

## Design notes

The template parameter `D` for `iterator_interface` may be an incomplete type.
Before any member of the resulting specialization of `iterator_interface`
other than special member functions is referenced, `D` must be complete, and
model `std::derived_from<iterator_interface<D>>`.

Since we're using CRTP, any operation that is provided by default that you do
not want, say because it has the wrong semantics, or because you know of a
more efficient way to do it, you can simply provided that operation in your
type, and the operation you would have inherited from the `iterator_interface`
gets hidden.

I did attempt to use `operator<=>` instead of the individually-defined
relational operators, but it created all kinds of difficult-to-sort-out
errors.

I also attempted to use deduced `this` in `iterator_interface`, but it did not
get rid of the need for the `D` template parameter. `D` is still needed to
define the hidden friend operators.
