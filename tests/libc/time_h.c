/* run.config
   STDOPT: #"-slevel 4"
 */

#include <time.h>



int main() {
  struct timespec req, rem;
  req.tv_sec = 42;
  req.tv_nsec = 9001;
  int r = nanosleep(&req, &rem);
  while (r) {
    if (errno == EINTR) {
      req = rem;
      r = nanosleep(&req, &rem);
    } else {
      return 1;
    }
  }
  r = nanosleep(&req, 0);
  if (r) return 2;

  struct timespec creq, crem;
  creq.tv_sec = 42;
  creq.tv_nsec = 9001;
  clock_nanosleep(CLOCK_REALTIME, TIMER_ABSTIME, &creq, &crem);
  //@ assert !\initialized(&crem);
  clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &creq, 0);

  r = clock_nanosleep(CLOCK_MONOTONIC, 0, &creq, &crem);
  while (r) {
    if (errno == EINTR) {
      creq = crem;
      r = clock_nanosleep(CLOCK_MONOTONIC, 0, &creq, &crem);
    } else {
      return 1;
    }
  }

  time_t tt = 42;
  char *time_str = ctime(&tt);
  //@ assert valid_string(time_str);
  return 0;
}
