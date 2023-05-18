#include <chrono>
#include <memory>
#include <mutex>
#include <queue>
#include <thread>

#include <csignal>
#include <cstdio>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

typedef std::vector<char> payload;

std::vector<std::vector<payload>> buffers;
std::vector<std::unique_ptr<std::mutex>> mtx;

int in = -1;
std::vector<int> out_fds;
std::string id;

void close_fds() {
  if (in != -1) close(in);
  for (int fd : out_fds) {
    close(fd);
  }
}

void fail_open(const char *f) {
  fprintf(stderr, "[%s] Could not open pipe %s: %s\n", id.c_str(), f, strerror(errno));
  exit(1);
}

void fail_read(int line) {
  fprintf(stderr, "[%s] Error on line %d when reading: %s\n", id.c_str(), line, strerror(errno));
  exit(1);
}

void fail_write(int line) {
  fprintf(stderr, "[%s] Error on line %d when writing: %s\n", id.c_str(), line, strerror(errno));
  exit(1);
}

void exit_eof() {
  fprintf(stderr, "[%s] Found EOF when reading, exiting now\n", id.c_str());
  exit(0);
}

void sanity_check(int64_t sz) {
  if (sz > (int64_t)1e9) {
    fprintf(stderr, "[%s] Read unreasonable input size: 0x%llx (%llu)\n", id.c_str(), sz, sz);
    exit(1);
  }
}

int64_t read_size() {
  char buf[sizeof(int64_t)];
  int64_t tot = 0;
  while (tot < sizeof(int64_t)) {
    int count = read(in, (void*)&buf[tot], sizeof(int64_t));
    if (count == 0) {
      if (tot > 0) {
        fprintf(stderr, "[%s] Got EOF after partially reading the size\n", id.c_str());
        exit(1);
      }
      return 0;
    }
    if (count < 0) fail_read(__LINE__);
    tot += count;
  }
  int64_t sz;
  memcpy((void*)&sz, buf, sizeof(int64_t));
  sanity_check(sz);
  return sz;
}

void read_task() {
  while (true) {
    payload p(read_size());
    if (p.size() == 0) {
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
      continue;
    }
    int64_t tot = 0;
    while (tot < p.size()) {
      int count = read(in, (void*)&p[tot], p.size()-tot);
      if (count == 0) {
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
      } else if (count < 0) {
        fail_read(__LINE__);
      }
      tot += count;
    }
    for (size_t i = 0; i < buffers.size(); i++) {
      mtx[i]->lock();
      buffers[i].push_back(p);
      mtx[i]->unlock();
    }
  }
}

void write_fd(const payload& p, size_t idx) {
  int out = out_fds[idx];
  int64_t tot = 0;
  while (tot < p.size()) {
    int count = write(out, (void*)&p[tot], p.size()-tot);
    if (count == 0) exit_eof();
    if (count < 0) fail_write(__LINE__);
    tot += count;
  }
}

payload combine_payloads(const std::vector<payload>& pls) {
  int64_t combined_sz = 0;
  for (const payload& p : pls) {
    combined_sz += p.size() + sizeof(int64_t);
  }
  payload combined(combined_sz);
  size_t pos = 0;
  for (const payload& p : pls) {
    int64_t sz = p.size();
    memcpy(&combined[pos], (void*)&sz, sizeof(int64_t));
    pos += sizeof(int64_t);
    memcpy(&combined[pos], &p[0], p.size());
    pos += p.size();
  }
  return combined;
}

std::vector<payload> read_buffer(int i) {
    mtx[i]->lock();
    std::vector<payload> pls(buffers[i].begin(), buffers[i].end());
    buffers[i].clear();
    mtx[i]->unlock();
    return pls;
}

void write_task(int i) {
  while (true) {
    while (buffers[i].empty()) {
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
      std::this_thread::yield();
    }
    std::vector<payload> pls = read_buffer(i);
    const payload& p = combine_payloads(pls);
    write_fd(p, i);
  }
}

void notify_termination(int sig) {
  int i = out_fds.size()-1;
  std::vector<payload> pls = read_buffer(i);
  if (!pls.empty()) {
    const payload &p = combine_payloads(pls);
    write_fd(p, out_fds.size()-1);
  }
  close_fds();
  exit(0);
}

void emergency_stop(int sig) {
  close_fds();
  exit(0);
}

int main(int argc, char **argv) {
  id = argv[1];
  in = open(id.c_str(), O_RDWR);
  if (in == -1) fail_open(id.c_str());
  for (int i = 2; i < argc; i++) {
    const char *dst = argv[i];
    int out = open(dst, O_RDWR);
    if (out == -1) fail_open(dst);
    out_fds.push_back(out);
  }
  signal(SIGINT, notify_termination);
  signal(SIGKILL, emergency_stop);

  mtx.resize(out_fds.size());
  buffers.resize(out_fds.size());
  for (size_t i = 0; i < out_fds.size(); i++) {
    mtx[i] = std::make_unique<std::mutex>();
    std::thread t(write_task, i);
    t.detach();
  }
  read_task();
  close_fds();
  return 0;
}
