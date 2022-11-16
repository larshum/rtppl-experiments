# Prevent matplotlib from launching window on every run
import matplotlib as mpl
mpl.use('Agg')

from PIL import Image
from datetime import datetime
import numpy as np
import matplotlib.pyplot as plt
import math
import os
import sys

def figid(x, n):
    c = chr(int(x % 26) + 65)
    if n > 1:
        return f"{figid(int(x / 26), int(n / 26))}{c}"
    else:
        return f"{c}"

batchsz = 1000

if not os.path.exists("plots/"):
    os.mkdir("plots")

if len(sys.argv) > 1:
    # Make a new directory based on current timestamp, so that we don't have to
    # overwrite old plots.
    now = datetime.now()
    target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
    os.mkdir(target)

    # Use a heatmap with 1cm precision
    roomFile = sys.argv[1]
    im = Image.open(roomFile)
    rows = im.height
    cols = im.width
    fig, axs = plt.subplots(1)
    axs.imshow(im)
    axs.set_xlabel("x")
    axs.set_ylabel("y")
    fig.savefig(f"{target}/0.png")
    plt.close()

    # Read the printed distribution from stdin and produce one image per n
    # outputs (corresponding to one inference iteration).
    inputs = [line for line in sys.stdin.readlines()]
    if len(inputs) % batchsz == 0:
        i = 0
        n = int(len(inputs) / batchsz)
        while i < len(inputs):
            fig, axs = plt.subplots(1)
            data = np.zeros([rows, cols])
            for line in inputs[i:i+batchsz]:
                # NOTE: we do not consider the angle when plotting
                x, y, _, w = line.split(" ")
                x = int(10 * float(x))
                y = int(10 * float(y))
                if x >= 0 and x < len(data[0]) and y >= 0 and y < len(data):
                    data[y][x] += math.exp(float(w))
            axs.imshow(data)
            axs.imshow(im, alpha=0.5)
            axs.set_xlabel("x")
            axs.set_ylabel("y")
            fig.savefig(f"{target}/{figid(int(i/batchsz), n)}.png")
            plt.close()
            i = i + batchsz
    else:
        print("Invalid number of input lines")
else:
    print("Invalid number of arguments")
