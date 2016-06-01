/* run.config
   OPT: -wp-rte -wp -wp-timeout 2
*/
/* run.config_qualif
   DONTRUN:
*/

typedef struct list { struct list *next; };
struct list *cur;

volatile int nondet;

void f(int i) {}

int main() {
  int bla = -1;
 reset:
  if (nondet) f(bla);
  while (nondet) {
    if (nondet) goto reset;
    if (nondet) goto exit;
  }
  goto reset;
 exit:
  while (nondet) {
    cur = cur->next;
  }
  return 0;
}
