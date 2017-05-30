#define _XOPEN_SOURCE 600
#include <sys/time.h>

int main() {
  struct itimerval i1 = {{1, 100}, {2, 200}};
  int res = setitimer(ITIMER_REAL, &i1, 0);
  //@ assert res == 0;
  struct itimerval i2;
  res = setitimer(ITIMER_REAL, &i1, &i2);
  //@ assert res == 0;
  //@ assert \initialized(&i2);
  res = getitimer(ITIMER_REAL, &i2);
  //@ assert res == 0;
  //@ assert \initialized(&i2);
  int INVALID_ITIMER = -1;
  res = getitimer(INVALID_ITIMER, &i2);
  //@ assert res == -1;
  i2.it_interval.tv_usec = 1000000; // invalid tv_usec
  res = setitimer(ITIMER_VIRTUAL, &i2, &i1);
  //@ assert res == -1;
  return 0;
}
