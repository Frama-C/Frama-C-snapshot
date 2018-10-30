/* run.config
   COMMENT: bts #2292, failures due to unregistered RTL functions
*/

#include <stdlib.h>

int a;
char *n = "134";
int main(int argc, char **argv) {
  a = argc = atoi(n);
}
