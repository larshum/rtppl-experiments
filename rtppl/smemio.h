#ifndef SMEMIO_H
#define SMEMIO_H

int open_sm(const char* str);

int read_sm(int fd, double *buffer, int n);

void write_sm(int fd, double *buffer, int n);

#endif
