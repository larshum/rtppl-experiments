import matplotlib as mpl
mpl.use('Agg')

from math import exp,log
from datetime import datetime
import itertools
import matplotlib.pyplot as plt
import numpy as np
import os
import struct
import shutil
import sys

import dist

src = sys.argv[1]
dists = dist.read_dists(src, 1)
if not os.path.exists("plots"):
    os.mkdir("plots")
w = 0.01
now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)
for i, dist in enumerate(dists):
    ts, samples = dist
    weights, values = zip(*samples)
    values = list(itertools.chain.from_iterable(values))
    fig, axs0 = plt.subplots(1)
    weights = np.array(list(weights))
    axs0.hist(values, bins=np.arange(0.0, max(values) + w, w), rwidth=0.9, weights=weights)
    axs0.set_xlabel("x")
    axs0.set_ylabel("probability")
    fig.savefig(f"{target}/{i:04}.png")
    plt.close()
