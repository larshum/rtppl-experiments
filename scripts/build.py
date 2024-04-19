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
