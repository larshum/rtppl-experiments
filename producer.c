#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "rtppl/smemio.h"

int main() {
  const char *file1 = "front-sensor-1";
  const char *file2 = "front-sensor-2";
  FILE *f1 = fopen(file1, "w+");
  FILE *f2 = fopen(file2, "w+");
  for (int i = 0; i < 10; i++) {
    int us = 10000 + rand() % 100000;
    usleep(us);
    double data1[2] = {rand() % 10, rand() % 100};
    double data2[2] = {rand() % 10, rand() % 100};
    write_sm(fileno(f1), data1, 2);
    write_sm(fileno(f2), data2, 2);
  }
  f1 = freopen(file1, "w", f1);
  fclose(f1);
  f2 = freopen(file2, "w", f2);
  fclose(f2);
  remove(file1);
  remove(file2);
}
