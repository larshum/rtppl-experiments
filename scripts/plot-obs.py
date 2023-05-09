import matplotlib as mpl
mpl.use('Agg')

from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
import os
import struct
import sys

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

obs = read_obs(sys.argv[1])
now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)
fig, axs0 = plt.subplots(1)
axs0.hist(obs, bins=np.arange(0.0, 2.0, 0.01))
axs0.set_xlabel("x")
axs0.set_ylabel("frequency")
fig.savefig(f"{target}/0.png")
