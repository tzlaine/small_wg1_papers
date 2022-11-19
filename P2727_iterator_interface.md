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

