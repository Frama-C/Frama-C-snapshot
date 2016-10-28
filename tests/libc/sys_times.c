#include <time.h>
#include <sys/times.h>

int main() {
  struct tms t;
  clock_t ck = times(&t);
  return 0;
}
