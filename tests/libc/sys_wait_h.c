/* run.config
   STDOPT: #"-slevel 4"
 */
#include <sys/wait.h>

int main() {
  pid_t r = wait(0);
  int stat_val;
  r = wait(&stat_val);
  if (r > 0) {
    //@ assert \initialized(&stat_val);
    if (WIFEXITED(stat_val))
      return 0;
  }
  r = waitpid(r, 0, 0);
  r = waitpid(r, &stat_val, WCONTINUED | WNOHANG | WUNTRACED);
  return 0;
}
