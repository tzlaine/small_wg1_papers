// g++-13 -std=c++23 conditionally_borrowed.cpp -o borrowed

#include "conditionally_borrowed.hpp"

#include <array>
#include <iostream>
#include <sstream>
#include <vector>


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

// TODO: concat_view

int main()
{
    std::vector<int> vec = {1, 2, 3, 4, 5, 6};
    std::vector<int> vec2 = {2, 3, 4, 5, 6, 1};

    {
        std::cout << "=== zip_transform ===\n\n";

        auto minus = [](auto x, auto y) { return x - y; };

        auto old_view = std::views::zip_transform(minus, vec, vec2);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = std::ranges::z::views::zip_transform(minus, vec, vec2);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = std::ranges::z::views::zip_transform(
            fat_callable(minus), vec, vec2);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== chunk_by ===\n\n";

        auto lt = [](auto x, auto y) { return x < y; };

        auto old_view = vec2 | std::views::chunk_by(lt);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto chunk : old_view) {
            for (auto x : chunk) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view = vec2 | std::ranges::z::views::chunk_by(lt);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto chunk : new_view) {
            for (auto x : chunk) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec2 | std::ranges::z::views::chunk_by(fat_callable(lt));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto chunk : fat_new_view) {
            for (auto x : chunk) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== join_with ===\n\n";

        using subrange_t = std::ranges::subrange<std::vector<int>::iterator>;
        std::vector<subrange_t> subranges = {
            {vec.begin(), vec.begin() + 2},
            {vec.begin() + 3, vec.begin() + 4},
            {vec.begin() + 4, vec.begin() + 6}};

        std::vector<int> pattern = {8, 9};

#if 0
        auto ints = std::istringstream{"1 2 3 4 5 6"};
        auto old_view = std::views::istream<int>(ints) | std::ranges::z::views::join_with(pattern);
#else
        auto old_view = subranges | std::views::join_with(pattern);
#endif
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = subranges | std::ranges::z::views::join_with(std::ranges::subrange(pattern));
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view_single = subranges | std::ranges::z::views::join_with(99);
        std::cout << "borrowed<new_view_single> = " << std::ranges::borrowed_range<decltype(new_view_single)> << "\n";
        std::cout << "sizeof(new_view_single.begin()) = " << sizeof(new_view_single.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view_single)>);
        for (auto x : new_view_single) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

#if 0
        auto ints = std::istringstream{"1 2 3 4 5 6"};
        auto input_view_single = std::views::istream<int>(ints) | std::ranges::z::views::join_with(99);
        std::cout << "borrowed<input_view_single> = " << std::ranges::borrowed_range<decltype(input_view_single)> << "\n";
        std::cout << "sizeof(input_view_single.begin()) = " << sizeof(input_view_single.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(input_view_single)>);
        for (auto x : input_view_single) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
#endif

        auto fat_new_view = subranges | std::ranges::z::views::join_with(pattern);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== adjacent_transform ===\n\n";

        auto minus = [](auto x, auto y) { return x - y; };

        auto old_view = vec | std::views::adjacent_transform<2>(minus);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view =
            vec | std::ranges::z::views::adjacent_transform<2>(minus);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = vec | std::ranges::z::views::adjacent_transform<2>(
                                      fat_callable(minus));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== filter ===\n\n";

        auto even = [](auto x) { return x % 2 == 0; };

        auto old_view = vec | std::views::filter(even);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::filter(even);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::filter(fat_callable(even));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== transform ===\n\n";

        auto negate = [](auto x) { return -x; };

        auto old_view = vec | std::views::transform(negate);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::transform(negate);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::transform(fat_callable(negate));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== take_while ===\n\n";

        auto lt_four = [](auto x) { return x < 4; };

        auto old_view = vec | std::views::take_while(lt_four);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(old_view)>);
        for (auto x : old_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto new_view = vec | std::ranges::z::views::take_while(lt_four);
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        auto new_it = new_view.begin();
        new_it += 1;
        for (auto x : new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";

        auto fat_new_view =
            vec | std::ranges::z::views::take_while(fat_callable(lt_four));
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        auto fat_new_it = fat_new_view.begin();
        fat_new_it += 1;
        for (auto x : fat_new_view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== lazy_split_view ===\n\n";

        std::vector<int> pattern({3});

        auto old_view = vec | std::views::lazy_split(pattern);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        for (auto subrng : old_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view =
            vec | std::ranges::z::views::lazy_split(std::ranges::subrange(pattern));
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto subrng : new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view_single = vec | std::ranges::z::views::lazy_split(3);
        std::cout << "borrowed<new_view_single> = " << std::ranges::borrowed_range<decltype(new_view_single)> << "\n";
        std::cout << "sizeof(new_view_single.begin()) = "
                  << sizeof(new_view_single.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view_single)>);
        for (auto subrng : new_view_single) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto ints = std::istringstream{"1 2 3 4 5 6"};
        auto input_view_single = std::views::istream<int>(ints) |
                                 std::ranges::z::views::lazy_split(3);
        std::cout << "borrowed<input_view_single> = " << std::ranges::borrowed_range<decltype(input_view_single)> << "\n";
        std::cout << "sizeof(input_view_single.begin()) = "
                  << sizeof(input_view_single.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(input_view_single)>);
        for (auto subrng : input_view_single) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = vec | std::ranges::z::views::lazy_split(pattern);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto subrng : fat_new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== split_view ===\n\n";

        std::vector<int> pattern({3});

        auto old_view = vec | std::views::split(pattern);
        std::cout << "borrowed<old_view> = " << std::ranges::borrowed_range<decltype(old_view)> << "\n";
        std::cout << "sizeof(old_view.begin()) = " << sizeof(old_view.begin()) << "\n";
        for (auto subrng : old_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view =
            vec | std::ranges::z::views::split(std::ranges::subrange(pattern));
        std::cout << "borrowed<new_view> = " << std::ranges::borrowed_range<decltype(new_view)> << "\n";
        std::cout << "sizeof(new_view.begin()) = " << sizeof(new_view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view)>);
        for (auto subrng : new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto new_view_single = vec | std::ranges::z::views::split(3);
        std::cout << "borrowed<new_view_single> = " << std::ranges::borrowed_range<decltype(new_view_single)> << "\n";
        std::cout << "sizeof(new_view_single.begin()) = "
                  << sizeof(new_view_single.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(new_view_single)>);
        for (auto subrng : new_view_single) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";

        auto fat_new_view = vec | std::ranges::z::views::split(pattern);
        std::cout << "borrowed<fat_new_view> = " << std::ranges::borrowed_range<decltype(fat_new_view)> << "\n";
        std::cout << "sizeof(fat_new_view.begin()) = " << sizeof(fat_new_view.begin()) << "\n";
        static_assert(!std::ranges::borrowed_range<decltype(fat_new_view)>);
        for (auto subrng : fat_new_view) {
            for (auto x : subrng) {
                std::cout << x << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }

    {
        std::cout << "=== join ===\n\n";

        using subrange_t = std::ranges::subrange<std::vector<int>::iterator>;
        std::vector<subrange_t> subranges = {
            {vec.begin(), vec.begin() + 2},
            {vec.begin() + 3, vec.begin() + 4},
            {vec.begin() + 4, vec.begin() + 6}};

        auto view = subranges | std::ranges::views::join;
        std::cout << "borrowed<view> = " << std::ranges::borrowed_range<decltype(view)> << "\n";
        std::cout << "sizeof(view.begin()) = " << sizeof(view.begin()) << "\n";
        static_assert(std::ranges::borrowed_range<decltype(view)>);
        for (auto x : view) {
            std::cout << x << "\n";
        }
        std::cout << "\n";
    }

    return 0;
}
