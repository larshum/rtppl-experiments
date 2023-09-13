import matplotlib as mpl
mpl.use('Agg')

from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
import os
import struct
import sys
import time

def read_collected(f):
    with open(f, "rb") as f:
        data = f.read()

    obs = []
    ofs = 0
    while ofs < len(data):
        sz, ts, v1, v2, v3 = struct.unpack("=qqqqq", data[ofs:ofs+40])
        obs.append((v1,v2,v3))
        ofs += 40
    return obs

def check_overrun(obs):
    for o in obs:
        if o[2] > 0:
            print(f"Task overran by {o[2]} ns")

def part_exec_pairs(obs):
    return zip(*[(o[1], o[0]) for o in obs])

now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)
for arg in sys.argv[1:]:
    print(arg)
    obs = read_collected(arg)
    check_overrun(obs)
    fig, axs0 = plt.subplots(1)
    name, _ = os.path.splitext(os.path.basename(arg))
    x, y = part_exec_pairs(obs)
    axs0.scatter(x, y)
    fig.savefig(f"{target}/{name}.png")
