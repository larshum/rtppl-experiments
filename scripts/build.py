import os
import subprocess
import sys

import network

def write_line(f, targets):
    f.write(f"{len(targets)} {' '.join(targets)}\n")

path = sys.argv[1]
os.chdir(path)
p = subprocess.run(["rtppl", "posmodel.rpl"] + sys.argv[2:])
if p.returncode != 0:
    print("RTPPL compilation failed")
    exit(1)
