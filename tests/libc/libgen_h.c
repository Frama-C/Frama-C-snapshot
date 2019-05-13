/*run.config

*/

#include <libgen.h>

int main() {
  char path[128] = "/tmp/bla/ble.c";
  char *base = basename(path);
  //@ assert valid_string(base);
  char *base2 = basename(0);
  //@ assert valid_string(base2);

  char *dir = dirname(path);
  //@ assert valid_string(dir);
  char *dir2 = dirname(0);
  //@ assert valid_string(dir2);

  return 0;
}
