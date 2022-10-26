from PIL import Image
import numpy as np
import matplotlib.pyplot as plt
import math
import sys

if len(sys.argv) > 1:
    # Use a heatmap with 1cm precision
    roomFile = sys.argv[1]
    im = Image.open(roomFile)
    rows = im.height * 10
    cols = im.width * 10

    # Read the printed distribution from stdin and produce one image per 1000
    # outputs (corresponding to one inference iteration).
    inputs = [line for line in sys.stdin.readlines()]
    if len(inputs) % 1000 == 0:
        i = 0
        while i < len(inputs):
            data = np.zeros([rows, cols])
            for line in inputs[i:i+1000]:
                x, y, _ = line.split(" ")
                x = int(100 * float(x))
                y = int(100 * float(y))
                data[x][y] += 1
            plt.imshow(data)
            plt.colorbar()
            plt.show()
            i = i + 1000
    else:
        print("Invalid number of input lines")
else:
    print("Invalid number of arguments")
