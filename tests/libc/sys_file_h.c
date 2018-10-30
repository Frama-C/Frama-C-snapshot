#include <fcntl.h>
#include <sys/file.h>

int main() {
  int fd = open("/tmp/bla", O_APPEND);
  int r = flock(fd, LOCK_SH);
  r = flock(fd, LOCK_UN);
  r = flock(fd, LOCK_EX);
  return 0;
}
