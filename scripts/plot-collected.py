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

def wcet_per_particle_count(obs):
    wcet = {}
    for o in obs:
        exec_time = o[0]
        p = o[1]
        if p in wcet:
            if exec_time > wcet[p]:
                wcet[p] = exec_time
        else:
            wcet[p] = exec_time
    return wcet

now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)
for arg in sys.argv[1:]:
    obs = read_collected(arg)
    check_overrun(obs)
    wcet = wcet_per_particle_count(obs)
    if len(wcet) > 0:
        fig, axs0 = plt.subplots(1)
        ordered = sorted(wcet.items())
        x, y = zip(*ordered)
        axs0.plot(x, y)
        axs0.set_xlabel("#particles")
        axs0.set_ylabel("worst-case execution time")
        name, _ = os.path.splitext(os.path.basename(arg))
        fig.savefig(f"{target}/{name}.png")
