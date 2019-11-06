/* run.config
   COMMENT: functions used in Contiki
*/

#include <stdlib.h>
#include <limits.h>

struct list {
  struct list *next;
  int value;
};

/*@
  logic integer length_aux(struct list *l, integer n) =
    n < 0 ? -1 :
      l == NULL ? n :
        n < INT_MAX ? length_aux(l->next, n+1) :
          -1;
  logic integer length(struct list *l) = length_aux(l, 0);
*/

int main (void) {
  struct list node1, node2, node3;
  node1.next = &node2;
  node2.next = &node3;
  struct list *l = &node1;
  /*@ assert length(l) == 3; */ ;
}