import json
import os
import subprocess
import sys

def write_line(f, targets):
    f.write(f"{len(targets)} {' '.join(targets)}\n")

path = sys.argv[1]
os.chdir(path)
rtppl_files = [f for f in os.listdir(".") if f.endswith(".rpl")]
if len(rtppl_files) == 0:
    print("Directory contains no RTPPL files")
    exit(1)
elif len(rtppl_files) == 1:
    rtppl_file = rtppl_files[0]
    p = subprocess.run(["rtppl", rtppl_file] + sys.argv[2:])
    if p.returncode != 0:
        print(f"RTPPL compilation failed for file {rtppl_file}")
        exit(1)
else:
    print("Directory contains multiple RTPPL files, expected one")
    exit(1)

# After compiling the ProbTime file, we set the minrate and maxrate parameters
# of each task in the configuration file. As all tasks of the system are
# periodic, we have that minrate = maxrate.
periods = {
    'speed' : 500 * 10**6,
    'distance_FC' : 500 * 10**6,
    'distance_SL' : 500 * 10**6,
    'distance_SR' : 500 * 10**6,
    'pos' : 10**9,
    'braking' : 500 * 10**6
}
with open("system.json", "r+") as f:
    data = json.load(f)
    for t in data['tasks']:
        p = periods[t['id']]
        t['minrate'] = p
        t['maxrate'] = p
    f.seek(0)
    json.dump(data, f)
    f.truncate()
