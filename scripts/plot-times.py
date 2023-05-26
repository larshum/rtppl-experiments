import matplotlib as mpl
mpl.use('Agg')

from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
import os
import struct
import sys
import time

now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)
vals = []
for path in sys.argv[1:]:
    with open(f"{path}/pos-logfile.txt", "r") as f:
        skipped_fst = False
        for line in f.readlines():
            if line.startswith("sdelay"):
                _, i = line.split()
                if skipped_fst:
                    vals.append(float(i) / 1e9)
                else:
                    skipped_fst = True
print(vals)
fig, axs0 = plt.subplots(1)
axs0.hist(vals, bins=np.arange(0.0, max(vals) + 0.05, 0.05))
axs0.set_xlabel("times")
axs0.set_ylabel("freq")
axs0.set_title(f"#samples={len(vals)}")
fig.savefig(f"{target}/times.png")
