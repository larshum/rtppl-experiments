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
  const char *src = "r1-outp";
  const char *dst = "r2-inp";
  int in = open(src, O_RDWR);
  if (in == -1) {
    fprintf(stderr, "Could not open %s for reading\n", src);
    exit(1);
  }
  int out = open(dst, O_RDWR);
  if (out == -1) {
    fprintf(stderr, "Could not open %s for writing\n", dst);
    exit(1);
  }
  struct payload buffer;
  while (1) {
    int count = read_payload(in, &buffer);
    if (count > 0) {
      count = write_payload(out, &buffer);
      if (count > 0) {
        fflush(stdout);
      }
    }
  }
  close(in);
  close(out);
  return 0;
}
