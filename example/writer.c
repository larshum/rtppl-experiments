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
  const char *destination = "r1-in1";
  int out = open(destination, O_RDWR | O_NONBLOCK);
  if (out == -1) {
    fprintf(stderr, "Could not open %s for writing\n", destination);
    exit(1);
  }
  struct payload buffer;
  buffer.ts = get_time();
  buffer.val = 0.0;
  while (1) {
    int count = write(out, (void*)&buffer, sizeof(struct payload));
    if (count > 0) {
      printf("wrote value: %lf with timestamp %lld\n", buffer.val, buffer.ts);
      fflush(stdout);
      buffer.val++;
    }
    int extra = rand() % (int)1e6;
    usleep((int)1e4 + extra);
    buffer.ts = get_time();
  }
  close(out);
  return 0;
}
