#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "common.h"

int main() {
  const char *src = "r1-out1";
  const char *dst = "r2-in1";
  int in = open(src, O_RDWR | O_NONBLOCK);
  if (in == -1) {
    fprintf(stderr, "Could not open %s for reading\n", src);
    exit(1);
  }
  int out = open(dst, O_RDWR | O_NONBLOCK);
  if (out == -1) {
    fprintf(stderr, "Could not open %s for writing\n", dst);
    exit(1);
  }
  struct payload buffer;
  while (1) {
    int count = read(in, (void*)&buffer, sizeof(buffer));
    if (count > 0) {
      count = write(out, (void*)&buffer, sizeof(buffer));
      if (count > 0) {
        fflush(stdout);
      }
    }
    usleep((int)1e4);
  }
  close(in);
  close(out);
  return 0;
}
