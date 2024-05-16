import matplotlib as mpl
mpl.use('Agg')

import matplotlib.pyplot as plt

from functools import cmp_to_key
import os
import statistics
import sys

import system

def contains_all_files(d):
    outputs = ["system.json", "log.txt", "pos.txt", "braking.txt"]
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

def read_task_wcet(file):
    with open(file, "r") as f:
        return max([int(l) for l in f.readlines()])

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

plt.rcParams.update({"font.size": 14})
fig, axs = plt.subplots(nrows=1, ncols=1, layout="constrained")
axs.grid(which="both")
axs.set_axisbelow(True)

fairness_kinds = ["execution-time", "particle"]
particle_x = [[], []]
particle_y = [[], []]
wcet_x = [[], []]
wcet_y = [[], []]
iterc = [[], []]
for a, b in ratios:
    x = a / b
    for i, fairness in enumerate(fairness_kinds):
        fdir = f"measurements/{fairness}/{a}-{b}"
        try:
            # Read the particle counts per task and compute the ratio
            particles = read_particles(f"{fdir}/system.json")
            y = particles['pos'] / particles['braking']
            particle_x[i].append(x)
            particle_y[i].append(y)

            # Read the WCETs per task and compute the ratio
            pos_wcet = read_task_wcet(f"{fdir}/pos.txt")
            braking_wcet = read_task_wcet(f"{fdir}/braking.txt")
            y = pos_wcet / braking_wcet
            wcet_x[i].append(x)
            wcet_y[i].append(y)

            iters = read_last_line(f"{fdir}/log.txt").strip()
            iterc[i].append(int(iters))
        except:
            pass

labels = [
  ["EF_P", "EF_WCET"],
  ["PF_P", "PF_WCET"]
]
fairness_color = ["orange", "blue"]
value_fmt = [
  ["+", "^"],
  ["x", "s"]
]
for i, fairness in enumerate(fairness_kinds):
    print(f"{fairness}: {statistics.mean(iterc[i])} +- {statistics.stdev(iterc[i])}")

    c = fairness_color[i]
    l1 = labels[i][0]
    l2 = labels[i][1]
    axs.plot(particle_x[i], particle_y[i], color=c, marker=value_fmt[i][0], label=l1, alpha=0.5)
    axs.plot(wcet_x[i], wcet_y[i], color=c, marker=value_fmt[i][1], label=l2, alpha=0.5)

axs.set_xlabel("Importance Ratio (pos / braking)")
axs.set_ylabel("Ratio of particles or WCET")
axs.set_xscale("log", base=2)
axs.set_yscale("log", base=2)
axs.set_xticks([2**i for i in range(-10, 12, 2)])
axs.set_yticks([2**i for i in range(-10, 12, 2)])
axs.legend(loc="upper left")
fig.savefig("auto-config.pdf",bbox_inches="tight")
