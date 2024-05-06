import sys

import system


def read_particles(f):
    sys = system.read_system(f)
    return {k: sys[k]['particles'] for k in sys['tasks'].keys()}


def read_last_line(file):
    with open(file, "r") as f:
        return f.readlines()[-1]


# We need to ensure that this is sufficiently large for our benchmark results,
# or add a function which finds this value automatically by considering the
# names of the output directories.
maxratio = 1024

pos = ("", "")
braking = ("", "")
iterations = ("", "")
column_headers = "Ratio"
column_format = "l"
for i, fairness in enumerate(["particle", "execution-time"]):
    fdir = f"measurements/{fairness}"
    # Loop through all ratios from the maximum imbalance toward pos (maxratio:1) to
    # the maximum imbalance toward the braking task (1:maxratio).
    a, b = maxratio, 1
    while a > 1 and b <= maxratio:
        try:
            particles = read_particles(f"{fdir}/{a}-{b}/system.json")
            pos[i] = f"{pos[i]} & {particles['pos']}"
            braking[i] = f"{braking[i]} & {particles['braking']}"
            iters = read_last_line(f"measurements/{fairness}/{a}-{b}/log.txt")
            iterations[i] = f"{iterations[i]} & {iters}"
            column_headers = f"{column_headers} & {a}:{b}"
            column_format = f"{column_format}c"
        except FileNotFoundError:
            print(f"Skipping {fairness} fairness with ratio {a}:{b}",
                  file=sys.stderr)
        if a > 1:
            a = a // 2
        else:
            b = b * 2

print(r"""\begin{{array}}{{{colformat}}}
{colheaders}\\
\hline
pos_{{E}}        {pos_particles_et}\\
braking_{{E}}    {braking_particles_et}\\
iterations_{{E}} {iterations_et}\\
\hline
pos_{{P}}        {pos_particles_pt}\\
braking_{{P}}    {braking_particles_pt}\\
iterations_{{P}} {iterations_pt}\\
\end{{array}}""".format(
    colformat=column_format,
    colheaders=column_headers,
    pos_particles_et=pos[0],
    pos_particles_pt=pos[1],
    braking_particles_et=braking[0],
    braking_particles_pt=braking[1],
    iterations_et=iterations[0],
    iterations_pt=iterations[1]
))
