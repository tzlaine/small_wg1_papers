#include <benchmark/benchmark.h>

#include <string>


void BM_foo(benchmark::State & state)
{
    while (state.KeepRunning()) {
        benchmark::DoNotOptimize(std::string_view());
    }
}

BENCHMARK(BM_foo);

BENCHMARK_MAIN()
