#include <mach/mach_time.h>
#include <mach/mach.h>
#include <mach/clock.h>

struct payload {
  int64_t ts;
  double val;
};

int read_payload(int fd, struct payload *p) {
  int64_t n;
  int c = read(fd, (void*)&n, sizeof(int64_t));
  if (c <= 0) {
    return c;
  }
  return read(fd, (void*)p, sizeof(struct payload));
}

int write_payload(int fd, struct payload *p) {
  int64_t n = sizeof(struct payload);
  int c = write(fd, (void*)&n, sizeof(int64_t));
  if (c <= 0) {
    return c;
  }
  return write(fd, (void*)p, sizeof(struct payload));
}

int get_timespec(struct timespec *ts) {
  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  ts->tv_sec = mts.tv_sec;
  ts->tv_nsec = mts.tv_nsec;
  return 0;
}

int64_t get_time() {
	struct timespec ts;
	get_timespec(&ts);
	return ts.tv_sec * (int64_t)1e9 + ts.tv_nsec;
}
