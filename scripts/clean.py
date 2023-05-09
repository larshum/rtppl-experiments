# Script for removing all generated files based on the contents of the
# generated network description file.

import json
import os
import sys

def try_remove(f):
    try:
        os.remove(f)
    except FileNotFoundError:
        pass

path = sys.argv[1]
nwfile = f"{path}/network.json"
with open(nwfile, "r") as f:
    data = json.load(f)

for task in data["tasks"]:
    try_remove(f"{path}/{task}")
    try_remove(f"{path}/{task}.mc")
for conn in data["connections"]:
    src, dst = conn["from"], conn["to"]
    try_remove(f"{path}/{src}")
    try_remove(f"{path}/{dst}")
try_remove(nwfile)
