// This file should be replaced with an implementation using shared memory.

#include "smemio.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int open_sm(const char *str) {
  FILE *f = fopen(str, "r+");
  if (f) {
    return fileno(f);
  } else {
    return -1;
  }
}

int read_sm(int fd, double *buffer, int n) {
  int r = read(fd, (void*)buffer, n * sizeof(double));
  lseek(fd, 0, SEEK_SET);
  return r;
}

void write_sm(int fd, double *buffer, int n) {
  write(fd, (void*)buffer, n * sizeof(double));
  lseek(fd, 0, SEEK_SET);
}
