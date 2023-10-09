import json

def read_network(file):
    with open(file) as f:
        data = json.loads(f.read())

    tasks = sorted(data["tasks"], key=lambda x: x["period"])

    # We only run the relay to deliver data between tasks
    relays = {}
    sensor_outs = {}
    actuator_ins = {}
    for c in data["connections"]:
        src = c["from"]
        dst = c["to"]
        if dst in data["actuators"]:
            if not dst in actuator_ins:
                actuator_ins[dst] = []
            actuator_ins[dst].append(src)
        elif src in data["sensors"]:
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
        "actuator_ins": actuator_ins
    }
