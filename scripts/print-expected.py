import dist
import os
import sys
from math import sqrt

posFile = "pos-posEst"
path = sys.argv[1]
if len(sys.argv) == 4:
    true_x = float(sys.argv[2])
    true_y = float(sys.argv[3])
else:
    true_x = None
    true_y = None
if os.path.isfile(path):
    pos_dists = sorted(dist.read_dists(path, 3))
    for ts, v in pos_dists:
        x, y, d = dist.compute_expected(v)
        print(f"{ts} | {x} {y} {d} ({len(v)})")
if true_x is not None:
    _, lastv = pos_dists[-1]
    x, y, _ = dist.compute_expected(lastv)
    print(f"Error: {sqrt((x-true_x)**2+(y-true_y)**2)}")
