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

if not os.path.isfile("relay"):
    subprocess.run(["g++", "relay.cpp", "-std=c++17", "-o", "relay"])

p = argparse.ArgumentParser()
p.add_argument("-p", "--path", action="store", required=True)
p.add_argument("-m", "--map", action="store", required=True)
p.add_argument("-r", "--replay", action="store")
p.add_argument("-u", "--usage", action="store_true")
args = p.parse_args()

map_file = args.map
path = args.path

nw = network.read_network(f"{path}/network.json")

def handler(sig, frame):
    print("Killing remaining processes")
    for proc in procs:
        if args.usage:
            cmd = ["ps", "-p", str(proc.pid), "-o", "%cpu,%mem"]
            p = subprocess.run(cmd, capture_output=True)
            print(proc.args, str(p.stdout))
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

if args.replay:
    subprocess.run(["python3", "scripts/clean.py", "-p", path])
    for _, dsts in nw["sensor_outs"].items():
        for dst in dsts:
            cmd = ["python3", "scripts/writer.py", f"{args.replay}/{dst}", f"{path}/{dst}"]
            print(cmd)
            procs.append(subprocess.Popen(cmd))

os.chdir(path)
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
    taskLog = open(f"{task}-logfile.txt", "w+")
    procs.append(subprocess.Popen(cmd, stdout=taskLog, env={"OCAMLRUNPARAM": "b"}))

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
