import json
import sys

lines = sys.stdin.readlines()
prop = lines[0].strip()
values = {}
for line in lines[1:]:
    if len(line.strip()) > 0:
        [task, value] = line.split(" ")
        if prop == "importance":
            values[task] = float(value)
        elif prop in ["core", "particles", "budget"]:
            values[task] = int(value)
        else:
            print(f"Unsupported property: {prop}")
            exit(1)

dst = sys.argv[1]
with open(f"{dst}/system.json", "r+") as f:
    data = json.load(f)
    tasks = data["tasks"]
    for task in tasks:
        if task["id"] in values:
            task[prop] = values[task["id"]]
    f.seek(0)
    json.dump(data, f)
    f.truncate()
