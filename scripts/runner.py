import argparse
import json
import os
import signal
import subprocess
import stat
import struct
import sys
import time

import network

def replay_messages(replay_path, target_path, sensor_outputs):
    # Read the raw data from all sensor files
    msgs = []
    for _, dsts in sensor_outputs:
        for dst in dsts:
            with open(f"{replay_path}/{dst}", "rb") as f:
                dst_file = open(f"{target_path}/{dst}", "wb")
                msgs.append((dst_file, f.read()))

    # Interpret the raw data so that we can order it by timestamps
    out_data = []
    for dst, payload in msgs:
        ofs = 0
        while ofs < len(payload):
            sz, ts = struct.unpack("=qq", payload[ofs:ofs+16])
            out_data.append((dst, ts, payload[ofs+16:ofs+sz+8]))
            ofs += sz + 8
    out_data = sorted(out_data, key=lambda x: x[1])

    # Replay the raw data by sending it out in the order it was observed in
    start_time = time.time_ns()
    t0 = out_data[0][1]
    for f, t1, payload in out_data:
        exec_time = time.time_ns() - start_time
        t_delta = t1 - t0
        if exec_time < t_delta:
            time.sleep((t_delta - exec_time) / 1e9)
        msg = struct.pack("=qq", len(payload) + 8, start_time + t_delta) + payload
        f.write(msg)
        f.flush()

    # Close the output files before quitting
    for f, _, _ in out_data:
        f.close()

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

signal.signal(signal.SIGINT, handler)

if args.replay:
    subprocess.run(["python3", "scripts/clean.py", "-p", path])

original_path = os.getcwd()
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

if args.replay:
    os.chdir(original_path)
    replay_messages(args.replay, path, nw["sensor_outs"].items())
    time.sleep(1)
    handler(signal.SIGINT, None)
else:
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
        time.sleep(0.1)
