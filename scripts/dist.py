import math
import struct

def read_dists(f, nfields):
    with open(f, mode="rb") as file:
        content = file.read()

    dists = []
    ofs = 0
    while ofs < len(content):
        sz, ts = struct.unpack("=qq", content[ofs:ofs+16])
        ofs += 16
        if len(content) - ofs < sz - 8:
            print(f"Warning: incomplete input from {f} after reading {len(dists)} distributions")
            break
        samplesz = (nfields + 1) * 8
        nsamples = int((sz - 8) / samplesz)
        samples = []
        for i in range(nsamples):
            w, = struct.unpack("d", content[ofs:ofs+8])
            ofs += 8
            v = []
            for j in range(nfields):
                x, = struct.unpack("d", content[ofs:ofs+8])
                v.append(x)
                ofs += 8
            samples.append((math.exp(w), v))
        dists.append((ts, samples))
    return dists
