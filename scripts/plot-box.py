import matplotlib as mpl
mpl.use('Agg')

import matplotlib.pyplot as plt
import numpy as np
import glob
import math

import dist

true_x = 2.41
true_y = 6.66

pcs = [100, 1000, 10000]#, 100000]
D = []
for p in pcs:
    data = []
    for path in glob.glob(f"measurements/{p}-particles/*/posDebug-actuator"):
        pos_dists = sorted(dist.read_dists(path, 3))
        _, d = pos_dists[-1]
        x, y, _ = dist.compute_expected(d)
        eucd = math.sqrt((x-true_x)**2+(y-true_y)**2)
        data.append(eucd)
    D.append(data)
labels = list(map(str, pcs))

fig, axs = plt.subplots(nrows=1, ncols=2, layout="constrained")
axs[0].boxplot(D, labels = labels)
axs[0].set_xlabel("#particles")
axs[0].set_ylabel("Error (m)")
axs[0].set_yscale("log")

x = np.arange(len(pcs))
barw = 0.2
multiple = 0

tasks = {
    "speedEst": [],
    "pos": [],
    "braking": []
}
for p in pcs:
    with open(f"measurements/{p}-particles/wcets.txt", "r") as f:
        s, p, b = f.read().strip().split(" ")
        tasks["speedEst"].append(float(s)/1e9)
        tasks["pos"].append(float(p)/1e9)
        tasks["braking"].append(float(b)/1e9)

for task, wcets in tasks.items():
    offset = barw * multiple
    rects = axs[1].bar(x + offset, wcets, barw, label=task)
    multiple += 1
axs[1].set_xticks(x + barw, labels)
axs[1].set_xlabel("#particles")
axs[1].set_ylabel("WCET (s)")
axs[1].set_yscale("log")
axs[1].legend(loc="upper left")

fig.savefig("pos-convergence.pdf",bbox_inches="tight")
