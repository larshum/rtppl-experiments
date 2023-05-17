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
import argparse

import dist

p = argparse.ArgumentParser()
p.add_argument("src", action="store")
p.add_argument("--lo", action="store", type=float, required=True)
p.add_argument("--hi", action="store", type=float, required=True)
p.add_argument("-w", action="store", type=float, required=True)
args = p.parse_args()

dists = dist.read_dists(args.src, 1)
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
    axs0.hist(values, bins=np.arange(args.lo, args.hi + args.w, args.w), rwidth=0.9, weights=weights)
    axs0.set_xlabel("x")
    axs0.set_ylabel("probability")
    fig.savefig(f"{target}/{i:04}.png")
    plt.close()
