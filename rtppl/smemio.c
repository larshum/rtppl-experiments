// This file should be replaced with an implementation using shared memory.

#include "smemio.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

void lv_read(int smemid, long long *ts, double *val) {
  // TODO: not implemented yet
}

void lv_write(int smemid, long long ts, double val) {
  // TODO: write values to the shared memory
}
