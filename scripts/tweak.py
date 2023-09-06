import argparse
import os
import subprocess

import network

def parse_infer_config(fname, l):
    tokens = l.strip().split(" ")
    if tokens[0] == "0":
        nparticles = int(tokens[1])
        return {"type": "fixed", "nparticles": nparticles}
    elif tokens[0] == "1":
        budget = int(tokens[1])
        max_particles = int(tokens[2])
        return {"type": "exec-budget", "budget": budget, "max-particles": max_particles}
    else:
        print(f"Invalid configuration file: {fname}")
        exit(1)

def read_config_file(fname):
    with open(fname, "r") as f:
        lines = f.readlines()
    collection = int(lines[0])
    infers = [parse_infer_config(fname, l) for l in lines[1:]]
    return {"collection-enabled" : collection, "infers" : infers}

def write_infer_config(f, infer):
    ty = infer["type"]
    if ty == "fixed":
        f.write(f"0 {infer['nparticles']}\n")
    elif ty == "exec-budget":
        f.write(f"1 {infer['budget']} {infer['max-particles']}\n")
    else:
        print(f"Invalid infer type: {ty}")
        exit(1)

def write_config_file(fname, config):
    with open(fname, "w+") as f:
        f.write(f"{config['collection-enabled']}\n")
        for infer in config["infers"]:
            write_infer_config(f, infer)

def remove_if_exists(fname):
    if os.path.exists(fname):
        os.remove(fname)

def default_config(ninfers):
    return {
        "collection-enabled" : 1,
        "infers" : [{"type": "fixed", "nparticles": 1} for i in range(ninfers)]
    }

p = argparse.ArgumentParser()
p.add_argument("-p", "--path", required=True)
p.add_argument("command", help="Command used to run all the tasks and update the collection files at the provided path")
args = p.parse_args()

prevpath = os.getcwd()

# 1. Run with default number of particles to find the worst-case execution
#    times of the mandatory parts of each task (i.e., everything except for the
#    infers).
os.chdir(args.path)
nw = network.read_network(f"network.json")
for task in nw["tasks"]:
    # Overwrite the configuration files of each task such that each infer runs
    # with one particle.
    task_id = task["id"]
    ninfers = task["infers"]
    config = default_config(ninfers)
    config_file = f"{task_id}.config"
    write_config_file(config_file, config)

os.chdir(prevpath)
subprocess.run(args.command.split(" "), capture_output=True)

os.chdir(args.path)
for task in nw["tasks"]:
    task_id = task['id']
    collect_file = f"{task_id}.collect"
    with open(collect_file, "r") as f:
        lines = f.readlines()
    exec_times = []
    for line in lines:
        toks = line.strip().split(" ")
        if toks[0] == "sdelay":
            exec_times.append(int(toks[1]))
    # TODO: Store this somewhere
    print(f"{task_id} WCET: {max(exec_times)}")
os.chdir(prevpath)

# 2. Given the worst-case execution times of the "mandatory" parts of each
# task, the user-provided importance of each task, and the task-to-node
# mapping, we run a sensitivity analysis to determine which tasks to
# prioritize.
pass

# 3. Iteratively run the provided command while updating the task
# configurations until we reach a point where all infers have been fixed.
pass

# 4. Disable the collection mode of all tasks and remove the collection files.
os.chdir(args.path)
for task in nw["tasks"]:
    # Disable collection mode
    task_id = task["id"]
    config_file = f"{task_id}.config"
    config = read_config_file(config_file)
    config.update({"collection-enabled" : 0})
    write_config_file(config_file, config)

    # Remove the collection files if they exist
    collection_file = f"{task_id}.collect"
    remove_if_exists(collection_file)
