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
  const char *source = "r2-out1";
  int in = open(source, O_RDWR | O_NONBLOCK);
  if (in == -1) {
    fprintf(stderr, "Could not open %s for reading\n", source);
    exit(1);
  }
  struct payload buffer;
  while (1) {
    int count;
    do {
      count = read(in, (void*)&buffer, sizeof(struct payload));
      if (count > 0) {
        printf("read value %lf at timestamp %lld\n", buffer.val, buffer.ts);
        fflush(stdout);
      }
    } while (count > 0);
    printf("=====\n");
    fflush(stdout);
    usleep((int)1e6);
  }
  close(in);
  return 0;
}
