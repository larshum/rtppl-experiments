import matplotlib as mpl
mpl.use('Agg')

import matplotlib.pyplot as plt
import numpy as np
import glob
import math
import sys

plt.rcParams.update({"font.size": 14})
fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, layout="constrained")

with open(sys.argv[1]) as f:
    [x, y] = f.read().split(" ")
    true_x, true_y = float(x), float(y)

# Plot the error along the x- and y-axis for each particle count as a violin
# plot on the left-hand side

pcs = [100, 1000, 10000, 100000]
X, Y = [], []
for i, _ in enumerate(pcs):
    euclid, x, y = [], [], []
    file = f"measurements/particles-{i}/accuracy.txt"
    with open(file) as f:
        for line in f.readlines():
            [xe, ye, _] = line.split(" ")
            x.append(float(xe))
            y.append(float(ye))
    X.append(x)
    Y.append(y)
labels = ["$10^2$", "$10^3$", "$10^4$", "$10^5$"]

xplot = ax1.violinplot(X)
yplot = ax1.violinplot(Y)

# Set the colors of the violins
for pc in xplot["bodies"]:
    pc.set_color("blue")
xplot["cbars"].set_color("blue")
xplot["cmins"].set_color("blue")
xplot["cmaxes"].set_color("blue")
for pc in yplot["bodies"]:
    pc.set_color("orange")
yplot["cbars"].set_color("orange")
yplot["cmins"].set_color("orange")
yplot["cmaxes"].set_color("orange")

# Set the labels in the plot
ax1.set_xlabel("#particles")
ax1.set_ylabel("Error (m)")
ax1.set_yscale("log")
ax1.set_xticks([1,2,3,4])
ax1.set_xticklabels(labels)
ax1.legend([xplot["bodies"][0], yplot["bodies"][0]], ["x-error", "y-error"], loc="upper right")

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
    rects = ax2.bar(x + offset, wcets, barw, label=task)
    multiple += 1
ax2.set_xticks(x + barw / 2, labels)
ax2.set_xlabel("#particles")
ax2.set_ylabel("WCET (s)")
ax2.set_yscale("log")
ax2.legend(loc="upper left")
ax2.yaxis.set_label_position("right")
ax2.yaxis.tick_right()

fig.set_figwidth(6.4)
fig.set_figheight(3.2)
fig.savefig("pos-convergence.pdf",bbox_inches="tight")
