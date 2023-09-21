import mmap
import os
import struct

BUFFER_SIZE = 2**20

buffers = {}

def open_file(fname):
    global buffers
    fd = os.open(fname, os.O_RDWR)
    os.ftruncate(fd, BUFFER_SIZE)
    buffers[fd] = mmap.mmap(fd, BUFFER_SIZE)
    return fd

def close_file(fd):
    os.close(fd)
    if fd in buffers:
        buffers[fd].close()
        buffers[fd] = None

# Attempt to read a message from the memory-mapped area and overwrite it with
# zeros after reading.
def read_message(fd):
    mm = buffers[fd]
    b = mm.read(8)
    sz, = struct.unpack("=q", b)
    mm.seek(-8, os.SEEK_CUR)
    if sz == 0:
        return None
    mm.write(bytearray(8))
    if mm.tell() + sz >= mm.size():
        n = mm.size() - mm.tell()
        b1 = mm.read(n)
        mm.seek(-n, os.SEEK_CUR)
        mm.write(bytearray(n))
        mm.seek(0, os.SEEK_SET)
        b2 = mm.read(sz - n)
        mm.seek(-(sz-n), os.SEEK_CUR)
        mm.write(bytearray(sz-n))
        return b1 + b2
    else:
        b = mm.read(sz)
        mm.seek(-sz, os.SEEK_CUR)
        mm.write(bytearray(sz))
        return b

def read_messages(fd):
    msgs = []
    while True:
        msg = read_message(fd)
        if msg is None:
            break
        msgs.append(msg)
    return msgs

def write_message(fd, msg):
    mm = buffers[fd]
    sz_pos = mm.tell()
    mm.seek(8, os.SEEK_CUR)
    if mm.tell() + len(msg) >= mm.size():
        n = mm.size() - mm.tell()
        mm.write(msg[:n])
        mm.seek(0, os.SEEK_SET)
        mm.write(msg[n:])
    else:
        mm.write(msg)
    new_pos = mm.tell()
    mm.seek(sz_pos, os.SEEK_SET)
    szbytes = struct.pack("=q", len(msg))
    mm.write(szbytes)
    mm.seek(new_pos)
    mm.flush()
