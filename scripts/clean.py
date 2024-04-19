# Script for removing all generated files based on the contents of the
# generated system description file.

import argparse
import os
import sys

import system

sysfile = "system.json"

def try_remove(f):
    try:
        os.remove(f)
    except FileNotFoundError:
        pass

def keep_file(clear_all_temp, nw, f):
    task_ids = []

    # We always want to keep source files
    if f.endswith(".rpl"):
        return True

    # If the '-a' flag is set, we remove all non-source files
    if clear_all_temp:
        return False

    # If it is a binary file, or the system specification file, we keep it to
    # avoid recompilation.
    bins = set([t["id"] for t in nw["tasks"]])
    if f in bins or f == sysfile:
        return True

    # If it is a task configuration file, we keep it to avoid having
    # re-configuring the number of particles to use in the task.
    configs = set([f"{t['id']}.config" for t in nw["tasks"]])
    if f in configs:
        return True

    # Otherwise, we don't need to keep it
    return False

p = argparse.ArgumentParser()
p.add_argument("-p", "--path", action="store", required=True)
p.add_argument("-a", action="store_true")
args = p.parse_args()

path = args.path
os.chdir(path)
nw = system.read_system(sysfile)

for f in os.listdir("."):
    if not keep_file(args.a, nw, f):
        try_remove(f)
