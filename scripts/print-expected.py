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
for x,_,_ in os.walk(path):
    p = f"{x}/pos-posEst"
    if os.path.isfile(p):
        print(f"Results of {x}")
        pos_dists = sorted(dist.read_dists(p, 3))
        for ts, v in pos_dists:
            x, y, d = compute_expected(v)
            print(f"{ts}| {x} {y} {d} ({len(v)})")
