import dist
import os
import sys

def compute_expected(v):
    x = 0.0
    y = 0.0
    d = 0.0
    for w, values in v:
        x = x + w * values[0]
        y = y + w * values[1]
        d = d + w * values[2]
    return x, y, d

posFile = "pos-posEst"
path = sys.argv[1]
if os.path.isfile(path):
    pos_dists = sorted(dist.read_dists(path, 3))
    for ts, v in pos_dists:
        x, y, d = compute_expected(v)
        print(f"{ts}| {x} {y} {d} ({len(v)})")
