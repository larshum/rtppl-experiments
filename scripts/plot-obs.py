import matplotlib as mpl
mpl.use('Agg')

from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
import os
import struct
import sys
import time

def read_obs(f):
    with open(f, "rb") as f:
        data = f.read()

    obs = []
    ofs = 0
    while ofs < len(data):
        _, _, v = struct.unpack("=qdd", data[ofs:ofs+24])
        obs.append(v)
        ofs += 24
    return obs

now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)
for idx, arg in enumerate(sys.argv[1:]):
    obs = read_obs(arg)
    fig, axs0 = plt.subplots(1)
    axs0.hist(obs, bins=np.arange(0.0, max(obs) + 0.05, 0.05))
    axs0.set_xlabel("x")
    axs0.set_ylabel("frequency")
    axs0.set_title(f"#samples={len(obs)}")
    name, _ = os.path.splitext(os.path.basename(arg))
    fig.savefig(f"{target}/{name}.png")
