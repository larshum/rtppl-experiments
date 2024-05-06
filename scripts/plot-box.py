import matplotlib as mpl
mpl.use('Agg')

import matplotlib.pyplot as plt
import numpy as np
import glob
import math
import sys

fig, axs = plt.subplots(nrows=1, ncols=2, layout="constrained")

with open(sys.argv[1]) as f:
    [x, y] = f.read().split(" ")
    true_x, true_y = float(x), float(y)

# Plot the accuracy for each particle count as a boxplot on the left-hand side

pcs = [100, 1000, 10000, 100000]
D = []
for i, _ in enumerate(pcs):
    data = []
    if i == 3:
        i = 4
    file = f"measurements/particles-{i}/accuracy.txt"
    with open(file) as f:
        for line in f.readlines():
            [_, _, err] = line.split(" ")
            data.append(float(err))
    D.append(data)
labels = ["$10^2$", "$10^3$", "$10^4$", "$10^5$"]
print(D, labels)

axs[0].boxplot(D, labels = labels)
axs[0].set_xlabel("#particles")
axs[0].set_ylabel("Error (m)")
axs[0].set_yscale("log")

# Plot the (worst-case) execution times for each particle count per task as a
# bar plot.
def find_task_wcet(i, task):
    wcet = 0.0
    for file in glob.glob(f"measurements/particles-{i}/*/{task}.collect"):
        with open(file) as f:
            wcet = max([wcet] + [float(x.strip())/1e9 for x in f.readlines()])
    return wcet

x = np.arange(len(pcs))
barw = 0.2
multiple = 0

tasks = {
    "pos": [],
    "braking": []
}
for i, _ in enumerate(pcs):
    for k, v in tasks.items():
        tasks[k].append(find_task_wcet(i, k))

for task, wcets in tasks.items():
    offset = barw * multiple
    rects = axs[1].bar(x + offset, wcets, barw, label=task)
    multiple += 1
axs[1].set_xticks(x + barw / 2, labels)
axs[1].set_xlabel("#particles")
axs[1].set_ylabel("WCET (s)")
axs[1].set_yscale("log")
axs[1].legend(loc="upper left")

fig.set_figwidth(6.4)
fig.set_figheight(3.2)
fig.savefig("pos-convergence.pdf",bbox_inches="tight")
