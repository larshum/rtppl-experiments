import os
import signal
import struct
import sys
import time

def signal_handler(sig, frame):
    exit(0)

def print_tsv(ts, v):
    print(ts, v)

def write_tsv(out, ts, v):
    ts = time.time_ns()
    bytes = struct.pack("=qqd", 16, ts, v)
    out.write(bytes)

signal.signal(signal.SIGINT, signal_handler)
file = sys.argv[1]
with open(file, "rb") as f:
    data = f.read()

if len(sys.argv) < 3:
    # write directly to stdout
    write_fn = lambda ts, v: print_tsv(ts, v)
else:
    # write to the specified file (typically a pipe)
    out = sys.argv[2]
    fd = os.open(out, os.O_RDWR)
    out = os.fdopen(fd, mode="wb", buffering=0)
    write_fn = lambda ts, v: write_tsv(out, ts, v)

ofs = 0
last_ts = 0
while ofs < len(data):
    if last_ts > 0:
        time.sleep((ts - last_ts) / 1e9)
        last_ts = ts
    else:
        time.sleep(0.1)
    _, ts, v = struct.unpack("=qqd", data[ofs:ofs+24])
    write_fn(ts, v)
    ofs += 24
print("End of observations")

if len(sys.argv) >= 3:
    os.close(fd)
