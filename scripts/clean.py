# Script for removing all generated files based on the contents of the
# generated network description file.

import argparse
import os
import sys

import network

def try_remove(f):
    try:
        os.remove(f)
    except FileNotFoundError:
        pass

def try_clear(f):
    try:
        with open(f, "w"):
            pass
    except FileNotFoundError:
        pass

p = argparse.ArgumentParser()
p.add_argument("-p", "--path", action="store", required=True)
p.add_argument("-a", action="store_true")
args = p.parse_args()

path = args.path
os.chdir(path)
nwfile = f"network.json"
data = network.read_network(nwfile)

# If the '-a' flag, we remove all files. Otherwise, we simply clear applicable
# files of contents.
if args.a:
    clear = try_remove
else:
    clear = try_clear

for _, dsts in data["sensor_outs"].items():
    for dst in dsts:
        clear(f"{dst}")
for src, dsts in data["relays"].items():
    clear(f"{src}")
    for dst in dsts:
        clear(f"{dst}")
for _, srcs in data["actuator_ins"].items():
    for src in srcs:
        clear(f"{src}")
for task in data["tasks"]:
    clear(f"{task['id']}-logfile.txt")

# These files should be removed when the '-a' flag is set, but otherwise we
# don't touch them.
if args.a:
    for src, _ in data["sensor_outs"].items():
        clear(f"{src}")
    for task in data["tasks"]:
        clear(f"{task['id']}")
        clear(f"{task['id']}.mc")
        clear(f"{task['id']}.config")
        clear(f"{task['id']}.collect")
        clear(f"{task['id']}.logfile")
    for dst, _ in data["actuator_ins"].items():
        clear(f"{dst}")
    clear(nwfile)
    clear("sa.txt")
    clear("true-values.txt")
