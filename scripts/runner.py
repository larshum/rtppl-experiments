import argparse
import json
import os
import signal
import subprocess
import stat
import sys
import time

import network

procs = []

def handler(sig, frame):
    print("Killing remaining processes");
    for proc in procs:
        proc.send_signal(signal.SIGINT)
        proc.send_signal(signal.SIGINT)
        try:
            proc.wait(0.5)
        except subprocess.TimeoutExpired:
            proc.send_signal(signal.SIGKILL)
            proc.terminate()
            proc.wait()
    sys.exit(0)

def ispipe(f):
    try:
        return stat.S_ISFIFO(os.stat(f).st_mode)
    except:
        return False

signal.signal(signal.SIGINT, handler)

if not os.path.isfile("relay"):
    subprocess.run(["g++", "relay.cpp", "-std=c++17", "-o", "relay"])

p = argparse.ArgumentParser()
p.add_argument("-p", "--path", action="store", required=True)
p.add_argument("-m", "--map", action="store", required=True)
args = p.parse_args()

map_file = args.map
path = args.path
os.chdir(path)
nw = network.read_network("network.json")
for src, dsts in nw["sensor_outs"].items():
    pass
for src, dsts in nw["relays"].items():
    cmd = ["../relay", src] + dsts
    print(cmd)
    procs.append(subprocess.Popen(cmd))
for dst, srcs in nw["actuator_ins"].items():
    pass
for task in nw["tasks"]:
    cmd = [f"./{task}", f"../{map_file}"]
    print(cmd)
    procs.append(subprocess.Popen(cmd, env={"OCAMLRUNPARAM": "b"}))

while True:
    live = []
    for proc in procs:
        if proc.poll():
            proc.kill()
            proc.wait()
            print(f"Process {proc.args} died")
        else:
            live.append(proc)
    procs = live
    time.sleep(1)
