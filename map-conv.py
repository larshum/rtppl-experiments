from PIL import Image
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
    im = Image.open(filename)
    print(f"{im.height} {im.width}")
    for y in range(im.height):
        for x in range(im.width):
            (r,g,b,_) = im.getpixel((x, y))
            if r == 0 and g == 0 and b == 0:
                print("1", end="")
            else:
                print("0", end="")
        print()
