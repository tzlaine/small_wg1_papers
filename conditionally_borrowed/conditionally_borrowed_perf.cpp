#include "conditionally_borrowed.hpp"

#include <benchmark/benchmark.h>

#include <algorithm>
#include <string>


template<typename F>
struct fat_callable
{
    template<typename... Args>
    auto operator()(Args &&... args) const
    {
        return f((Args &&) args...);
    }
    F f;
    std::array<int, 256> extra_bytes;
};

auto make_int_vec(int n)
{
    std::vector<int> retval(n);
    std::ranges::generate(retval, std::rand);
    return retval;
}

auto plus = [](auto x, auto y) { return x + y; };

void BM_zip_transform_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = std::views::zip_transform(plus, vec, vec);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_zip_transform_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = std::ranges::z::views::zip_transform(plus, vec, vec);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_zip_transform_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view =
        std::ranges::z::views::zip_transform(fat_callable(plus), vec, vec);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

int plus_ptr(int x, int y) { return x + y; }

void BM_ptr_zip_transform_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = std::views::zip_transform(&plus_ptr, vec, vec);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_zip_transform_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = std::ranges::z::views::zip_transform(&plus_ptr, vec, vec);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_zip_transform_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view =
        std::ranges::z::views::zip_transform(fat_callable(&plus_ptr), vec, vec);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

auto lt = [](auto x, auto y) { return x < y; };

void BM_chunk_by_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::chunk_by(lt);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_chunk_by_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::chunk_by(lt);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_chunk_by_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::chunk_by(fat_callable(lt));
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}

bool lt_ptr(int x, int y) { return x < y; }

void BM_ptr_chunk_by_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::chunk_by(&lt_ptr);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_ptr_chunk_by_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::chunk_by(&lt_ptr);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_ptr_chunk_by_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::chunk_by(fat_callable(&lt_ptr));
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}

auto identity = [](auto x) { return x; };

void BM_join_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view = subranges | std::views::join | std::views::transform(identity) |
                std::views::split(99) | std::views::join;
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_join_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view = subranges | std::ranges::z::views::join |
                std::views::transform(identity) |
                std::ranges::z::views::split(99) | std::ranges::z::views::join;
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

void BM_join_with_old_owned_pattern(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<int> pattern({99});

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view = subranges | std::views::join_with(pattern);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_join_with_old_subrange_pattern(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<int> pattern({99});

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view =
        subranges | std::views::join_with(std::ranges::subrange(pattern));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_join_with_old_single(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view = subranges | std::ranges::views::join_with(99);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_join_with_new_owned_pattern(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<int> pattern({99});

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view = subranges | std::ranges::z::views::join_with(pattern);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_join_with_new_subrange_pattern(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<int> pattern({99});

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view = subranges | std::ranges::z::views::join_with(
                                std::ranges::subrange(pattern));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_join_with_new_single(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    using subrange_t = std::ranges::subrange<std::vector<int>::const_iterator>;

    std::vector<subrange_t> subranges;
    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    int back = 0;
    while (back < state.range(0)) {
        int lo = back + std::rand() % rand_range;
        lo = (std::min)(lo, state.range(0));
        int hi = lo + std::rand() % rand_range;
        hi = (std::min)(hi, state.range(0));
        back = hi;
        subranges.push_back(subrange_t{vec.begin() + lo, vec.begin() + hi});
    }

    auto out = std::vector<int>(state.range(0) * 5);
    auto view = subranges | std::ranges::z::views::join_with(99);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

void BM_adjacent_transform_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::adjacent_transform<2>(plus);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_adjacent_transform_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::adjacent_transform<2>(plus);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_adjacent_transform_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view =
        vec | std::ranges::z::views::adjacent_transform<2>(fat_callable(plus));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

void BM_ptr_adjacent_transform_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::adjacent_transform<2>(&plus_ptr);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_adjacent_transform_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::adjacent_transform<2>(&plus_ptr);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_adjacent_transform_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::adjacent_transform<2>(
                          fat_callable(&plus_ptr));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

auto even = [](auto x) { return x % 2 == 0; };

void BM_filter_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::filter(even);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_filter_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::filter(even);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_filter_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::filter(fat_callable(even));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

bool even_ptr(int x) { return x % 2 == 0; }

void BM_ptr_filter_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::filter(&even_ptr);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_filter_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::filter(&even_ptr);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_filter_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::filter(fat_callable(&even_ptr));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

auto negate = [](auto x) { return -x; };

void BM_transform_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::transform(negate);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_transform_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::transform(negate);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_transform_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::transform(fat_callable(negate));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

int negate_ptr(int x) { return -x; }

void BM_ptr_transform_old(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::transform(&negate_ptr);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_transform_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::transform(&negate_ptr);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_ptr_transform_fat_new(benchmark::State & state)
{
    auto const vec = make_int_vec(state.range(0));
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::transform(fat_callable(&negate_ptr));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

void BM_take_while_old(benchmark::State & state)
{
    int half = 0;
    auto lt_half = [&](auto x) { return x < half; };
    auto vec = make_int_vec(state.range(0));
    std::ranges::sort(vec);
    half = vec[vec.size() / 2];
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::take_while(lt_half);
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
struct lt_half
{
    bool operator()(int x) const { return x < half_; }
    int half_;
};
void BM_take_while_new(benchmark::State & state)
{
    int half = 0;
    auto vec = make_int_vec(state.range(0));
    std::ranges::sort(vec);
    half = vec[vec.size() / 2];
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::take_while(lt_half(half));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}
void BM_take_while_fat_new(benchmark::State & state)
{
    int half = 0;
    auto lt_half = [&](auto x) { return x < half; };
    auto vec = make_int_vec(state.range(0));
    std::ranges::sort(vec);
    half = vec[vec.size() / 2];
    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::take_while(fat_callable(lt_half));
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::ranges::copy(view, out.begin()));
    }
}

void BM_lazy_split_old_owned_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::lazy_split(pattern);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_lazy_split_old_subrange_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::lazy_split(std::ranges::subrange(pattern));
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_lazy_split_old_single(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::views::lazy_split(99);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_lazy_split_new_owned_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::lazy_split(pattern);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_lazy_split_new_subrange_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view =
        vec | std::ranges::z::views::lazy_split(std::ranges::subrange(pattern));
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_lazy_split_new_single(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::lazy_split(99);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}

void BM_split_old_owned_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::split(pattern);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_split_old_subrange_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::views::split(std::ranges::subrange(pattern));
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_split_old_single(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::views::split(99);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_split_new_owned_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::split(pattern);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_split_new_subrange_pattern(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view =
        vec | std::ranges::z::views::split(std::ranges::subrange(pattern));
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}
void BM_split_new_single(benchmark::State & state)
{
    auto vec = make_int_vec(state.range(0));

    std::vector<int> pattern({99});

    const int rand_range = state.range(0) < 100 ? 3 : state.range(0) / 10;
    auto dest = vec.begin();
    while (dest != vec.end()) {
        auto prev_dest = dest;
        std::ranges::advance(dest, std::rand() % rand_range, vec.end());
        std::fill(prev_dest, dest, 0);
        if (dest != vec.end())
            *dest++ = 99;
    }

    auto out = std::vector<int>(state.range(0));
    auto view = vec | std::ranges::z::views::split(99);
    while (state.KeepRunning()) {
        auto dest = out.begin();
        for (auto && chunk : view) {
            benchmark::DoNotOptimize(dest = std::ranges::copy(chunk, dest).out);
        }
    }
}

#define POWER_10_ARGS                                                          \
    ->Arg(10)->Arg(100)->Arg(1000)->Arg(10000)->Arg(100000)->Arg(1000000)

BENCHMARK(BM_zip_transform_old) POWER_10_ARGS;
BENCHMARK(BM_zip_transform_new) POWER_10_ARGS;
BENCHMARK(BM_zip_transform_fat_new) POWER_10_ARGS;

BENCHMARK(BM_ptr_zip_transform_old) POWER_10_ARGS;
BENCHMARK(BM_ptr_zip_transform_new) POWER_10_ARGS;
BENCHMARK(BM_ptr_zip_transform_fat_new) POWER_10_ARGS;

BENCHMARK(BM_chunk_by_old) POWER_10_ARGS;
BENCHMARK(BM_chunk_by_new) POWER_10_ARGS;
BENCHMARK(BM_chunk_by_fat_new) POWER_10_ARGS;

BENCHMARK(BM_ptr_chunk_by_old) POWER_10_ARGS;
BENCHMARK(BM_ptr_chunk_by_new) POWER_10_ARGS;
BENCHMARK(BM_ptr_chunk_by_fat_new) POWER_10_ARGS;

BENCHMARK(BM_join_old) POWER_10_ARGS;
BENCHMARK(BM_join_new) POWER_10_ARGS;

BENCHMARK(BM_join_with_old_owned_pattern) POWER_10_ARGS;
BENCHMARK(BM_join_with_old_subrange_pattern) POWER_10_ARGS;
BENCHMARK(BM_join_with_old_single) POWER_10_ARGS;
BENCHMARK(BM_join_with_new_owned_pattern) POWER_10_ARGS;
BENCHMARK(BM_join_with_new_subrange_pattern) POWER_10_ARGS;
BENCHMARK(BM_join_with_new_single) POWER_10_ARGS;

BENCHMARK(BM_adjacent_transform_old) POWER_10_ARGS;
BENCHMARK(BM_adjacent_transform_new) POWER_10_ARGS;
BENCHMARK(BM_adjacent_transform_fat_new) POWER_10_ARGS;

BENCHMARK(BM_ptr_adjacent_transform_old) POWER_10_ARGS;
BENCHMARK(BM_ptr_adjacent_transform_new) POWER_10_ARGS;
BENCHMARK(BM_ptr_adjacent_transform_fat_new) POWER_10_ARGS;

BENCHMARK(BM_filter_old) POWER_10_ARGS;
BENCHMARK(BM_filter_new) POWER_10_ARGS;
BENCHMARK(BM_filter_fat_new) POWER_10_ARGS;

BENCHMARK(BM_ptr_filter_old) POWER_10_ARGS;
BENCHMARK(BM_ptr_filter_new) POWER_10_ARGS;
BENCHMARK(BM_ptr_filter_fat_new) POWER_10_ARGS;

BENCHMARK(BM_transform_old) POWER_10_ARGS;
BENCHMARK(BM_transform_new) POWER_10_ARGS;
BENCHMARK(BM_transform_fat_new) POWER_10_ARGS;

BENCHMARK(BM_ptr_transform_old) POWER_10_ARGS;
BENCHMARK(BM_ptr_transform_new) POWER_10_ARGS;
BENCHMARK(BM_ptr_transform_fat_new) POWER_10_ARGS;

BENCHMARK(BM_take_while_old) POWER_10_ARGS;
BENCHMARK(BM_take_while_new) POWER_10_ARGS;
BENCHMARK(BM_take_while_fat_new) POWER_10_ARGS;

BENCHMARK(BM_lazy_split_old_owned_pattern) POWER_10_ARGS;
BENCHMARK(BM_lazy_split_old_subrange_pattern) POWER_10_ARGS;
BENCHMARK(BM_lazy_split_old_single) POWER_10_ARGS;
BENCHMARK(BM_lazy_split_new_owned_pattern) POWER_10_ARGS;
BENCHMARK(BM_lazy_split_new_subrange_pattern) POWER_10_ARGS;
BENCHMARK(BM_lazy_split_new_single) POWER_10_ARGS;

BENCHMARK(BM_split_old_owned_pattern) POWER_10_ARGS;
BENCHMARK(BM_split_old_subrange_pattern) POWER_10_ARGS;
BENCHMARK(BM_split_old_single) POWER_10_ARGS;
BENCHMARK(BM_split_new_owned_pattern) POWER_10_ARGS;
BENCHMARK(BM_split_new_subrange_pattern) POWER_10_ARGS;
BENCHMARK(BM_split_new_single) POWER_10_ARGS;

BENCHMARK_MAIN()
