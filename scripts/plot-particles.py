import matplotlib as mpl
mpl.use('Agg')

from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
import os
import struct
import sys
import time

import dist

now = datetime.now()
target = f'plots/{now.strftime("%Y%m%d-%H%M%S")}'
os.mkdir(target)
path = sys.argv[1]
skipped_fst = False
vals = []
for _,d in dist.read_dists(f"{path}/pos-posEst", 3):
    if skipped_fst:
        vals.append(len(d))
    else:
        skipped_fst = True
print(vals)
fig, axs0 = plt.subplots(1)
axs0.hist(vals, bins=np.arange(0.0, max(vals), 100))
axs0.set_xlabel("times")
axs0.set_ylabel("freq")
axs0.set_title(f"#samples={len(vals)}")
fig.savefig(f"{target}/particles.png")
