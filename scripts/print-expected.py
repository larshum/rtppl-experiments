import dist
import os
import sys
from math import sqrt

posFile = "pos-posEst"
path = sys.argv[1]
if len(sys.argv) == 3:
    with open(sys.argv[2]) as f:
        x, y = f.read().strip().split(" ")
    true_x, true_y = float(x), float(y)
else:
    true_x = None
    true_y = None
if os.path.isfile(path):
    pos_dists = sorted(dist.read_dists(path, 3))
else:
    exit(1)
if true_x is not None:
    _, lastv = pos_dists[-1]
    x, y, _ = dist.compute_expected(lastv)
    err_x = sqrt((x - true_x)**2)
    err_y = sqrt((y - true_y)**2)
    err = sqrt((x - true_x)**2 + (y - true_y)**2)
    print(f"{err_x} {err_y} {err}")
else:
    for ts, v in pos_dists:
        x, y, d = dist.compute_expected(v)
        print(f"{ts} | {x} {y} {d} ({len(v)})")
