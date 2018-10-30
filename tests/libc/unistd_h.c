/*run.config
  STDOPT:
  STDOPT: #"-variadic-no-translation"
*/
#include <unistd.h>

#define _XOPEN_SOURCE 600

volatile int nondet;

int main() {
  int r = usleep(123);
  r = usleep(456);
  char hostname[256];
  r = gethostname(hostname, 256);
  // Note: the value set by gethostname is NOT guaranteed to be null-terminated
  execl("/bin/sh", "sh", "-c", "ls", (char*)0);

  r = access("/tmp", R_OK | W_OK | X_OK);
  //@ assert r == -1 || r == 0;

  int fd = dup(1);
  //@ assert fd == -1 || fd >= 0;
  if (fd == -1) return 1;

  int fd2 = dup2(2, fd);
  if (nondet) {
    dup2(2, -2);
    //@ assert unreachable: \false;
  }

  int pid = fork(); // note: process creation not modeled by Eva
  //@ assert pid == -1 || pid >= 0;

  r = setsid();

  sync();

  long l = sysconf(ARG_MAX);

  return 0;
}
