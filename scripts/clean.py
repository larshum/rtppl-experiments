# Script for removing all generated files based on the contents of the
# generated network description file.

import os
import sys

import network

def try_remove(f):
    try:
        os.remove(f)
    except FileNotFoundError:
        pass
    try:
        os.remove(f"{f}.txt")
    except FileNotFoundError:
        pass

path = sys.argv[1]
os.chdir(path)
nwfile = f"network.json"
data = network.read_network(nwfile)

for src, dsts in data["sensor_outs"].items():
    try_remove(f"sensor-{src}")
    for dst in dsts:
        try_remove(f"{dst}")
for src, dsts in data["relays"].items():
    try_remove(f"{src}")
    for dst in dsts:
        try_remove(f"{dst}")
for task in data["tasks"]:
    try_remove(f"{task}")
    try_remove(f"{task}.mc")
for dst, srcs in data["actuator_ins"].items():
    try_remove(f"actuator-{dst}")
    for src in srcs:
        try_remove(f"{src}")
try_remove(nwfile)
try_remove("sa.txt")
try_remove("true-values.txt")
