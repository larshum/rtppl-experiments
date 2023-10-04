import mmap
import os
import struct
from multiprocessing import shared_memory

BUFFER_SIZE = 2**20

buffers = {}

def open_file(fname):
    global buffers
    try:
        shm = shared_memory.SharedMemory(name=fname, create=True, size=BUFFER_SIZE)
    except:
        shm = shared_memory.SharedMemory(name=fname)
    buffers[shm] = 0
    return shm

def close_file(shm):
    shm.close()
    if shm in buffers:
        buffers[shm] = None

# Attempt to read a message from the memory-mapped area and overwrite it with
# zeros after reading.
def read_message(shm):
    pos = buffers[shm]
    b = shm.buf[pos:pos+8]
    sz, = struct.unpack("=q", b)
    if sz <= 0:
        return None
    shm.buf[pos:pos+8] = bytearray(8)
    pos += 8
    if pos + sz >= len(shm.buf):
        n = len(shm.buf) - pos
        b1 = bytes(shm.buf[pos:n])
        shm.buf[pos:] = bytearray(n)
        b2 = bytes(shm.buf[0:sz-n])
        shm.buf[0:sz-n] = bytearray(sz-n)
        buffers[shm] = sz-n
        return b1 + b2
    else:
        b = bytes(shm.buf[pos:pos+sz])
        shm.buf[pos:pos+sz] = bytearray(sz)
        buffers[shm] = pos+sz
        return b

def read_messages(shm):
    msgs = []
    while True:
        msg = read_message(shm)
        if msg is None:
            break
        msgs.append(msg)
    return msgs

def write_message(shm, msg):
    sz_pos = buffers[shm]
    pos = sz_pos + 8
    if pos + len(msg) >= len(shm.buf):
        n = len(shm.buf) - pos
        shm.buf[pos:] = bytes(msg[:n])
        shm.buf[0:len(msg)-n] = bytes(msg[n:])
        buffers[shm] = len(msg)-n
    else:
        shm.buf[pos:pos+len(msg)] = bytes(msg)
        buffers[shm] = pos+len(msg)
    szbytes = struct.pack("=q", len(msg))
    shm.buf[sz_pos:sz_pos+8] = szbytes
