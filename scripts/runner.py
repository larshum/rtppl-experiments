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
import mmio

def combine_tasks_with_core_mapping(tasks, task_to_core_map_file):
    try:
        with open(task_to_core_map_file, "r") as f:
            ttcm = {}
            for line in f.readlines():
                [task, core] = line.strip().split(" ")
                ttcm[task] = int(core)
        for task in tasks:
            task['core'] = ttcm[task['id']]
    except FileNotFoundError:
        for task in tasks:
            task['core'] = 1
    return tasks

def replay_messages(replay_path, target_path, sensor_outputs):
    # Read the raw data from all sensor files
    msgs = []
    for _, dsts in sensor_outputs:
        for dst in dsts:
            try:
                with open(f"{replay_path}/{dst}", "rb") as f:
                    dst_fd = mmio.open_file(dst)
                    msgs.append((dst_fd, f.read()))
            except:
                # Do not provide any data if the file was not found
                pass

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
    for fd, t1, payload in out_data:
        exec_time = time.time_ns() - start_time
        t_delta = t1 - t0
        if exec_time < t_delta:
            time.sleep((t_delta - exec_time) / 1e9)
        msg = struct.pack("=qq", len(payload) + 8, start_time + t_delta) + payload
        mmio.write_message(fd, msg)

    # Close the output files before quitting
    fds = set([fd for fd, _, _ in out_data])
    for fd in fds:
        mmio.close_file(fd)

def store_debug_pos(debug_file):
    shm, f = debug_file
    msgs = mmio.read_messages(shm)
    data = b''.join(msgs)
    f.write(data)

procs = []

p = argparse.ArgumentParser()
p.add_argument("-p", "--path", action="store", required=True)
p.add_argument("-m", "--map", action="store", required=True)
p.add_argument("-r", "--replay", action="store")
p.add_argument("-u", "--usage", action="store_true")
p.add_argument("--record", action="store_true")
args = p.parse_args()

map_file = args.map
path = args.path

nw = network.read_network(f"{path}/network.json")

debug_file = None

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
    if debug_file:
        shm, f = debug_file
        mmio.close_file(shm)
        f.close()
    sys.exit(0)

signal.signal(signal.SIGINT, handler)

if args.replay:
    subprocess.run(["python3", "scripts/clean.py", "-p", path])

original_path = os.getcwd()
os.chdir(path)

tasks = combine_tasks_with_core_mapping(nw["tasks"], "task-core-map.txt")
priority = 99
for task in tasks:
    cmd = [f"./{task['id']}", f"../{map_file}"]
    cmd = ["taskset", "-c", f"{task['core']}"] + cmd
    print(cmd)
    taskLog = open(f"{task['id']}-logfile.txt", "w+")
    proc = subprocess.Popen(cmd, stdout=taskLog, env={"OCAMLRUNPARAM": "b"})
    os.sched_setscheduler(proc.pid, os.SCHED_FIFO, os.sched_param(priority))
    priority -= 1
    procs.append(proc)

if args.replay:
    os.chdir(original_path)
    replay_messages(args.replay, path, nw["sensor_outs"].items())
    time.sleep(1)
    for proc in procs:
        if proc.poll():
            proc.kill()
            proc.wait()
            print(f"Process {proc.args} died: {proc.stdout}\n{proc.stderr}")
    handler(signal.SIGINT, None)
else:
    if args.record:
        shm = mmio.open_file("posDebug")
        fd = open("posDebug", "wb")
        debug_file = (shm, fd)
    while True:
        if args.record:
            store_debug_pos(debug_file)
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
