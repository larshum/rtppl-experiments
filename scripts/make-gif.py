import matplotlib as mpl
mpl.use('Agg')

from PIL import Image
from datetime import datetime
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import argparse
import os

import dist

def read_pos_dists(f):
    return sorted(dist.read_dists(f, 3), key=lambda x: x[0])

p = argparse.ArgumentParser()
p.add_argument("-m", "--map", action="store", required=True)
p.add_argument("-p", "--path", action="store", required=True)
args = p.parse_args()

im = Image.open(args.map)
rows = im.height
cols = im.width

pos = read_pos_dists(f"{args.path}/pos-posEst")

now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)

fig, ax = plt.subplots()
ax.set_xlabel("x")
ax.set_ylabel("y")

def plot_frame(idx):
    _, samples = pos[idx]
    expected = (0.0, 0.0)
    data = np.zeros([rows, cols])
    for w, s in samples:
        expected = (expected[0] + 10 * s[0] * w, expected[1] + 10 * s[1] * w)
        x = int(10*s[0])
        y = int(10*s[1])
        if x >= 0 and x < cols and y >= 0 and y < rows:
            data[y][x] += 1
    ax.clear()
    ax.imshow(data)
    ax.plot(expected[0], expected[1], 'bo', markersize=1)
    ax.imshow(im, alpha=0.5)

ani = animation.FuncAnimation(fig=fig, func=plot_frame, frames=len(pos), interval=200)
ani.save(filename=f"{target}/position.gif", writer="pillow")
