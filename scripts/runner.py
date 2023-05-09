import json
import os
import signal
import subprocess
import sys
import time

procs = []

def handler(sig, frame):
    print("Killing remaining processes");
    for proc in procs:
        proc.send_signal(signal.SIGINT)
        try:
            proc.wait(0.5)
        except subprocess.TimeoutExpired:
            proc.terminate()
            proc.wait()
    sys.exit(0)

def read_network(file):
    with open(file) as f:
        data = json.loads(f.read())

    # We ignore the brake task and any connections involving it
    tasks = data["tasks"]

    # We only run the relay to deliver data between tasks
    relays = {}
    sensor_outs = {}
    actuator_ins = {}
    for c in data["connections"]:
        src = c["from"]
        dst = c["to"]
        if src in data["sensors"]:
            if not src in sensor_outs:
                sensor_outs[src] = []
            sensor_outs[src].append(dst)
        elif dst in data["actuators"]:
            if not dst in actuator_ins:
                actuator_ins[dst] = []
            actuator_ins[dst].append(src)
        else:
            if not src in relays:
                relays[src] = []
            relays[src].append(dst)

    return {
        "tasks": tasks,
        "relays": relays,
        "sensor_outs": sensor_outs,
        "actuator_ins": actuator_ins
    }

signal.signal(signal.SIGINT, handler)

if not os.path.isfile("relay"):
    subprocess.run(["g++", "relay.cpp", "-std=c++17", "-o", "relay"])

map_file = sys.argv[1]
path = sys.argv[2]
os.chdir(path)
nw = read_network("network.json")
for src, dsts in nw["relays"].items():
    cmd = ["../relay", src] + dsts
    print(cmd)
    procs.append(subprocess.Popen(cmd, stdin=None, stdout=None, stderr=None))
for src, dsts in nw["sensor_outs"].items():
    pass
    #for dst in dsts:
    #    cmd = ["python3", "scripts/writer.py", f"car-data/{src}.txt", dst]
    #    print(cmd)
    #    procs.append(subprocess.Popen(cmd, stdin=None, stdout=None, stderr=None))
for src, dsts in nw["actuator_ins"].items():
    pass
for task in nw["tasks"]:
    cmd = [f"./{task}", f"../{map_file}"]
    print(cmd)
    procs.append(subprocess.Popen(cmd, stdin=None, stdout=None, stderr=None, env={"OCAMLRUNPARAM": "b"}))

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
