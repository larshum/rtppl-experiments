import os
import subprocess
import sys

import network

def write_line(f, targets):
    f.write(f"{len(targets)} {' '.join(targets)}\n")

path = sys.argv[1]
os.chdir(path)
p = subprocess.run(["rtppl", "posmodel.rpl", "--print-infer"])
if p.returncode != 0:
    print("RTPPL compilation failed")
    exit(1)

nw = network.read_network("network.json")
sensors = nw["sensor_outs"]
with open("sa.txt", "w+") as f:
    write_line(f, sensors["frontLeft"])
    write_line(f, sensors["frontRight"])
    write_line(f, sensors["rearLeft"])
    write_line(f, sensors["rearRight"])
    write_line(f, sensors["sideLeft"])
    write_line(f, sensors["sideRight"])
    write_line(f, sensors["speedLeft"])
    write_line(f, sensors["speedRight"])
    write_line(f, sensors["steeringAngle"])
    write_line(f, nw["actuator_ins"]["brake"])

