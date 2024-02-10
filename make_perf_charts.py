#!/usr/bin/env python3

import itertools
import json
import math
import pygal

import argparse

parser = argparse.ArgumentParser(description='Takes data from a Google Benchmark JSON file, and makes a bunch of pretty pictires.')
parser.add_argument('json_file', type=str, help='Google Benchmark JSON file to proccess.')
args = parser.parse_args()

f = open(args.json_file, 'r')
samples = json.load(f)

samples = samples['benchmarks']

styles = {}

def get_style(run_names):
    key = ''.join(run_names)
    return styles[key]

styles[''.join(['Old', 'New', 'Fat New'])] = \
    pygal.style.Style(
        background='transparent',
        plot_background='transparent',
        colors=('#FF0000', '#00FF00', '#007F7F'))
styles[''.join(['Old Owned Pattern', 'Old Subrange Pattern', 'Old Single',
                'New Owned Pattern', 'New Subrange Pattern', 'New Single'])] = \
    pygal.style.Style(
        background='transparent',
        plot_background='transparent',
        colors=('#FF0000', '#FF3F00', '#FF7F00', '#00FF00', '#007F7F', '#0000FF'))

def next_largest_power_10(x):
    return 10 ** math.ceil(math.log10(x))

def chunk(r, n = 1):
    l = len(r)
    for i in range(0, l, n):
        yield r[i:min(i + n, l)]

def common_prefix(names):
    zipped = zip(*curr_run_names)
    prefixes = itertools.takewhile(lambda x: all(x[0] == y for y in x), zipped)
    return ''.join(x[0] for x in prefixes)

def names_and_runs(run, run_names):
    prefix = common_prefix(run_names)
    name = prefix[:-1] if prefix.endswith('_') else prefix
    run_names = map(lambda x: x[len(prefix):], run_names)
    run_names = list(map(lambda x: ' '.join(map(lambda y: y.title(), x.split('_'))), run_names))
    runs = list(chunk(run, len(all_iterations)))
    return (name, run_names, runs)

def print_chart(run, run_names):
    name, run_names, runs = names_and_runs(run, run_names)
    style = get_style(run_names)
    chart = pygal.Line(
        style=style,
        logarithmic=True,
        y_title='Nanoseconds',
        x_title='Number of Elements',
        tooltip_border_radius=10,
        legend_at_bottom=True)
    chart.title = f'{name}_view'
    chart.x_labels = list(map(lambda x: str(x), all_iterations))
    for name_,run_ in zip(run_names, runs):
        # print(name, name_, run_)
        chart.add(name_, run_, show_dots=False)
    with open(f'{name}.svg', 'w') as out_f:
        out_f.write(str(chart.render(2)))

def print_table(run, run_names):
    name, run_names, runs = names_and_runs(run, run_names)
    label = 'Run \\ `N`'
    longest = max(map(lambda x: len(x), run_names + [label]))
    sep = '+-' + '-' * longest + '-+'
    hdr = f'| {label:{longest}} |'
    hdr_sep = '+=' + '=' * longest + '=+'
    for x in all_iterations:
        sep += '-' * 9 + '+'
        hdr += f'{x:8} |'
        hdr_sep += ':' + '=' * 7 + ':+'
    print(f'=== {name} ===')
    print(sep)
    print(hdr)
    print(hdr_sep)

    for name_,run_ in zip(run_names, runs):
        row = f'| {name_:{longest}} |'
        for x in run_:
            row += f'{x:8} |'
        print(row)
        print(sep)

    print('\n')


all_iterations = []

runs = {}
prev_name = ''
prev_prefix = ''
prev_iterations = 10 ** 10
curr_run = []
curr_run_names = []
for sample in samples:
    name = sample['name'][3:] # Carve off 'BM_' prefix.
    name,iterations = name.split('/')
    if iterations.endswith('k'):
        iterations = next_largest_power_10(float(iterations[:-1]) * 1024)
    else:
        iterations = int(iterations)
    if iterations not in all_iterations:
        all_iterations.append(iterations)
    cpu_time = int(sample['cpu_time'])
    prefix = name.split('_')[0]

    if prefix != prev_prefix and len(curr_run):
        print_chart(curr_run, curr_run_names)
        print_table(curr_run, curr_run_names)
        curr_run = []
        curr_run_names = []
    curr_run.append(cpu_time)
    if iterations < prev_iterations:
        curr_run_names.append(name)
    prev_name = name
    prev_prefix = prefix
    prev_iterations = iterations
print_chart(curr_run, curr_run_names)
print_table(curr_run, curr_run_names)
