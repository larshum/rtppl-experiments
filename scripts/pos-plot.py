# Prevent matplotlib from launching window on every run
import matplotlib as mpl
mpl.use('Agg')

from PIL import Image
from datetime import datetime
import bisect
import numpy as np
import matplotlib.pyplot as plt
import math
import os
import sys
import subprocess

def figid(x, n):
    c = chr(int(x % 26) + 65)
    if n > 1:
        return f"{figid(int(x / 26), int(n / 26))}{c}"
    else:
        return f"{c}"

def read_float(f):
    r = subprocess.run(["./out", "--print-float", f], capture_output=True, text=True)
    return r.stdout.splitlines()

def read_pos_dist(f):
    r = subprocess.run(["./out", "--print-pos-dist", f], capture_output=True, text=True)
    return r.stdout.splitlines()

batchsz = 1000

if not os.path.exists("plots/"):
    os.mkdir("plots")

if len(sys.argv) > 4:
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

    # Read the input data and the reference positions
    x_trace = read_float(sys.argv[2])
    y_trace = read_float(sys.argv[3])
    pos = []
    for (l1, l2) in zip(x_trace, y_trace):
        ts, x = l1.split(" ")
        _, y = l2.split(" ")
        pos.append((float(x), float(y), int(ts)))

    pos_trace = read_pos_dist(sys.argv[4])
    inputs = [line for line in pos_trace]

    # Read the printed distribution from stdin and produce one image per n
    # outputs (corresponding to one inference iteration).
    if len(inputs) % (batchsz + 1) == 0:
        i = 0
        n = int(len(inputs) / (batchsz + 1))
        while i < len(inputs):
            fig, axs = plt.subplots(1)
            data = np.zeros([rows, cols])
            ts = int(inputs[i])
            idx = bisect.bisect(pos, ts, key=lambda k: k[2])
            if idx == len(pos):
                true_pos = pos[-1]
            else:
                true_pos = pos[idx]
            true_x = int(10 * true_pos[0])
            true_y = int(10 * true_pos[1])
            for line in inputs[i+1:i+(batchsz+1)]:
                # NOTE: we do not consider the angle when plotting
                x, y, _, w = line.split(" ")
                x = int(10 * float(x))
                y = int(10 * float(y))
                if x >= 0 and x < len(data[0]) and y >= 0 and y < len(data):
                    data[y][x] += 1
            axs.imshow(data)

            old_pixel = im.getpixel((true_x, true_y))
            im.putpixel((true_x, true_y), (255, 0, 0, 255))
            axs.imshow(im, alpha=0.5)
            im.putpixel((true_x, true_y), old_pixel)

            axs.set_xlabel("x")
            axs.set_ylabel("y")
            fig.savefig(f"{target}/{figid(int(i/batchsz), n)}.png")
            plt.close()
            i = i + batchsz + 1
    else:
        print(f"Invalid number of input lines: {len(inputs)}")
else:
    print("Invalid number of arguments")
