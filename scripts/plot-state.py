# Prevent matplotlib from launching a window on every run
import matplotlib as mpl
mpl.use('Agg')

from PIL import Image
from datetime import datetime
import numpy as np
import matplotlib.pyplot as plt
import math
import os
import struct
import subprocess
import sys

import dist

class Dist:
    def __init__(self, x, y, d):
        self.x = x
        self.y = y
        self.direction = d

def figid(x):
    c = chr(int(x % 26) + 65)
    if x >= 26:
        return f"{figid(int(x / 26))}{c}"
    else:
        return f"{c}"

def conv_dist(dist):
    ts, samples = dist
    conv_samples = []
    for sample in samples:
        w, (x, y, direction) = sample
        conv_samples.append((w, Dist(x, y, direction)))
    return (ts, conv_samples)

def read_state_dists(f):
    return [conv_dist(dist) for dist in dist.read_dists(f, 3)]

if not os.path.exists("plots/"):
    os.mkdir("plots")

if len(sys.argv) == 3:
    now = datetime.now()
    target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
    os.mkdir(target)

    roomFile = sys.argv[1]
    im = Image.open(roomFile)
    rows = im.height
    cols = im.width
    fig, axs = plt.subplots(1)
    axs.imshow(im)
    axs.set_xlabel("x")
    axs.set_ylabel("y")
    fig.savefig(f"{target}/0000.png")
    plt.close()

    # TODO: read reference positions

    dists = read_state_dists(sys.argv[2])
    for i, (_ts, dist) in enumerate(dists):
        fig, axs = plt.subplots(1)
        data = np.zeros([rows, cols])
        for (w, s) in dist:
            # NOTE: assumes each pixel of the map represents a 10x10 cm square
            x = int(10 * s.x)
            y = int(10 * s.y)
            if x >= 0 and x < cols and y >= 0 and y < rows:
                data[y][x] += 1
        axs.imshow(data)
        axs.set_xlabel("x")
        axs.set_ylabel("y")
        axs.imshow(im, alpha=.5)
        fig.savefig(f"{target}/{i+1:04}.png")
        plt.close()
