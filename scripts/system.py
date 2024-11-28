import json

def read_system(file):
    with open(file, "r") as f:
        data = json.load(f)

    tasks = sorted(data["tasks"], key=lambda x: x["minarrival"])
    sensors = [x["id"] for x in data["sensors"]]
    actuators = [x["id"] for x in data["actuators"]]

    # We only run the relay to deliver data between tasks
    relays = {}
    sensor_outs = {}
    actuator_ins = {}
    for c in data["connections"]:
        src = c["from"]
        dst = c["to"]
        if dst in actuators:
            if not dst in actuator_ins:
                actuator_ins[dst] = []
            actuator_ins[dst].append(src)
        elif src in sensors:
            if not src in sensor_outs:
                sensor_outs[src] = []
            sensor_outs[src].append(dst)
        else:
            if not src in relays:
                relays[src] = []
            relays[src].append(dst)

    return {
        "tasks": tasks,
        "relays": relays,
        "sensor_outs": sensor_outs,
        "actuator_ins": actuator_ins,
        "compileopts": data["compileopts"]
    }
