import math
import struct

def compute_expected(v):
    x = 0.0
    y = 0.0
    d = 0.0
    for w, values in v:
        x = x + w * values[0]
        y = y + w * values[1]
        d = d + w * values[2]
    return x, y, d

def read_dists(f, nfields):
    try:
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
    except:
        print(f"Could not find values for {f}")
        return []
