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

map_file = sys.argv[1]
path = sys.argv[2]
os.chdir(path)
nw = network.read_network("network.json")
for src, dsts in nw["sensor_outs"].items():
    src = f"sensor-{src}"
    if not ispipe(src):
        os.mkfifo(src)
    cmd = ["../relay", src] + dsts
    print(cmd)
    procs.append(subprocess.Popen(cmd))
for src, dsts in nw["relays"].items():
    cmd = ["../relay", src] + dsts
    print(cmd)
    procs.append(subprocess.Popen(cmd))
for dst, srcs in nw["actuator_ins"].items():
    dst = f"actuator-{dst}"
    if not ispipe(dst):
        os.mkfifo(dst)
    cmd = ["../relay", srcs[0], dst]
    print(cmd)
    procs.append(subprocess.Popen(cmd))
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
