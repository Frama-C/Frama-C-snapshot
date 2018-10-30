/* run.config
   STDOPT: #"-slevel 2"
*/
#include <signal.h>

volatile int nondet;

int main() {
  sigset_t s;
  if (sigemptyset(&s)) return 1;
  if (sigaddset(&s, SIGALRM)) {
    return -1;
  }
  if (sigdelset(&s, SIGUSR1)) {
    return -1;
  }
  if (!sigismember(&s, SIGALRM)) return 2;
  sigfillset(&s);
  if (!sigismember(&s, SIGPIPE)) return 3;
  sigset_t uninit;
  if (nondet) {
    if (sigaddset(&uninit, SIGKILL)) {
      return -1;
    }
    //@ assert unreachable_if_precise: \false;
  }

  sigset_t old;
  if (sigprocmask(SIG_SETMASK, 0, &old)) {
    return -1;
  }
  if (sigaddset(&old, SIGALRM)) {
    return -1;
  }
  if (sigprocmask(SIG_SETMASK, &old, 0)) {
    return -1;
  }
  if (sigprocmask(SIG_BLOCK, &s, &old)) {
    return -1;
  }

  int kill_res = kill(42, SIGTERM);

  return 0;
}
