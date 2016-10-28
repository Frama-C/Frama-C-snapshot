#include <unistd.h>

#define _XOPEN_SOURCE 600

int main() {
  int r = usleep(123);
  r = usleep(456);
  return 0;
}
