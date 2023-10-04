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
    if os.path.isfile(f):
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

# If the '-a' flag is set, we remove all files except for the original source
# files. Otherwise, we also keep the configuration files.
for f in os.listdir("."):
    if not f.endswith(".rpl"):
        if not args.a and not f.endswith(".config"):
            try_remove(f)
