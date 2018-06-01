/*run.config
  STDOPT:
  STDOPT: #"-variadic-no-translation"
*/
#include <unistd.h>

#define _XOPEN_SOURCE 600

int main() {
  int r = usleep(123);
  r = usleep(456);
  char hostname[256];
  r = gethostname(hostname, 256);
  // Note: the value set by gethostname is NOT guaranteed to be null-terminated
  execl("/bin/sh", "sh", "-c", "ls", (char*)0);
  return 0;
}
