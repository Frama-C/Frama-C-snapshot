/* run.config*
   STDOPT: #""
*/
#include <string.h>

typedef struct {
  int e;
  char *obval;
  long long c;
} ob;

typedef struct {
  long long a;
  int b;
  char stval[2];
} st;

void main() {
  ob o;
  o.obval = "a";
  o.c = 32;
  char *p = ((st *)&o)->stval;
  char k = *p;
  size_t z = strlen(p);
}
