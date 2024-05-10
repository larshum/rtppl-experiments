from functools import cmp_to_key
import os
import statistics
import sys

import system

def contains_all_files(d):
    outputs = ["system.json", "log.txt", "accuracy.txt"]
    return all([os.path.isfile(f"{d}/{f}") for f in outputs])

def cmp_ratios(x, y):
    if x[0] > 1 and y[0] > 1:
        return y[0] - x[0]
    if x[0] > 1 and y[0] == 1:
        return -1
    if x[0] == 1 and y[0] > 1:
        return 1
    if x[0] == 1 and y[0] == 1:
        if x[1] > 1 and y[1] > 1:
            return x[1] - y[1]
        if x[1] > 1 and y[1] == 1:
            return 1
        if x[1] == 1 and y[1] > 1:
            return -1
        return 0

def read_particles(f):
    sys = system.read_system(f)
    return {t['id']: t['particles'] for t in sys['tasks']}

def read_last_line(file):
    with open(file, "r") as f:
        return f.readlines()[-1]

def read_accuracy(file):
    def stats(v):
        return f"{statistics.mean(v):.2f} \pm {statistics.stdev(v):.2f}"
    with open(file, "r") as f:
        x, y, err = [], [], []
        for line in f.readlines():
            [xv, yv, errv] = line.split(" ")
            x.append(float(xv))
            y.append(float(yv))
            err.append(float(errv))
        return (stats(x), stats(y), stats(err))

# Collect all ratios present in the files produced by the particle and
# execution-time fairness
ratios = set()
for i, fairness in enumerate(["particle", "execution-time"]):
    fdir = f"measurements/{fairness}"
    for f in os.listdir(fdir):
        if contains_all_files(f"{fdir}/{f}"):
            [a, b] = f.split("-")
            ratios.add((int(a), int(b)))
ratios = sorted(list(ratios), key=cmp_to_key(cmp_ratios))

pos = ["", ""]
braking = ["", ""]
iterations = ["", ""]
x_err = ["", ""]
y_err = ["", ""]
err = ["", ""]
column_headers = r"\text{{Ratio}}"
column_format = "l"
fairness_kinds = ["execution-time", "particle"]
for a, b in ratios:
    if a == 1 and b == 1:
        header = "1:1"
    elif a > 1:
        header = f"{a}:"
    else:
        header = f":{b}"
    column_headers = f"{column_headers} & {header}"
    column_format = f"{column_format}c"
    for i, fairness in enumerate(fairness_kinds):
        fdir = f"measurements/{fairness}/{a}-{b}"
        try:
            particles = read_particles(f"{fdir}/system.json")
            pos[i] = f"{pos[i]} & {particles['pos']}"
            braking[i] = f"{braking[i]} & {particles['braking']}"
            iters = read_last_line(f"{fdir}/log.txt").strip()
            iterations[i] = f"{iterations[i]} & {iters}"
            x, y, e = read_accuracy(f"{fdir}/accuracy.txt")
            x_err[i] = f"{x_err[i]} & {x}"
            y_err[i] = f"{y_err[i]} & {y}"
            err[i] = f"{err[i]} & {e}"
        except FileNotFoundError:
            pos[i] = f"{pos[i]} & "
            braking[i] = f"{braking[i]} & "
            iterations[i] = f"{iterations[i]} & "
            x_err[i] = f"{x_err[i]} & "
            y_err[i] = f"{y_err[i]} & "
            err[i] = f"{err[i]} & "

print(r"""\[
\begin{{array}}{{{colformat}}}
{colheaders}\\
\hline
\text{{pos}}_{{E}}        {pos_particles_et}\\
\text{{braking}}_{{E}}    {braking_particles_et}\\
\text{{iterations}}_{{E}} {iterations_et}\\
\hline
\text{{pos}}_{{P}}        {pos_particles_pt}\\
\text{{braking}}_{{P}}    {braking_particles_pt}\\
\text{{iterations}}_{{P}} {iterations_pt}\\
\end{{array}}
\]""".format(
    colformat=column_format,
    colheaders=column_headers,
    pos_particles_et=pos[0],
    pos_particles_pt=pos[1],
    braking_particles_et=braking[0],
    braking_particles_pt=braking[1],
    iterations_et=iterations[0],
    iterations_pt=iterations[1],
))
