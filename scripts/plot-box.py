import matplotlib as mpl
#mpl.use('Agg')

import matplotlib.pyplot as plt
import numpy as np
import glob
import math

import dist

true_x = 2.41
true_y = 6.66

pcs = [100, 1000, 10000, 100000]
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

fig, axs = plt.subplots(1)
axs.boxplot(D, labels = labels)
axs.set_xlabel("#particles")
axs.set_ylabel("Error (m)")
axs.set_yscale("log")
plt.show()
#fig.savefig("pos-convergence.pdf",bbox_inches="tight")
