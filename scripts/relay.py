from threading import Lock, Thread
import signal
import sys
import time

import mmio

buffers = []
mutexes = []
threads = []

infile = None
outfiles = []
running = True

def close_files():
    global infile, outfiles
    if infile != -1:
        mmio.close_file(infile)
        infile = -1
    for outfile in outfiles:
        mmio.close_file(outfile)
    outfiles = []

def read_task():
    while running:
        msgs = mmio.read_messages(infile)
        if len(msgs) == 0:
            time.sleep(0.01)
            continue
        for i in range(len(outfiles)):
            mutexes[i].acquire()
            buffers[i].extend(msgs)
            mutexes[i].release()

def read_buffer(i):
    mutexes[i].acquire()
    data = b"".join(buffers[i])
    buffers[i] = []
    mutexes[i].release()
    return data

def write_task(i):
    while running:
        if len(buffers[i]) == 0:
            time.sleep(0.01)
            continue
        msg = read_buffer(i)
        mmio.write_message(outfiles[i], msg)

def notify_termination(sig, frame):
    global running
    running = False
    close_files()
    for t in threads:
        t.join()
    sys.exit(0)

signal.signal(signal.SIGINT, notify_termination)

infile = mmio.open_file(sys.argv[1])
for dst in sys.argv[2:]:
    outfiles.append(mmio.open_file(dst))

mutexes = [Lock() for i in outfiles]
buffers = [[] for i in outfiles]
for i, outf in enumerate(outfiles):
    t = Thread(target=write_task, args=[i])
    threads.append(t)
    t.start()
read_task()
