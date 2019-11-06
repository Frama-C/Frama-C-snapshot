/* run.config
   COMMENT: allocation and de-allocation of local variables
*/

#include <stdlib.h>

struct list {
  int element;
  struct list * next;
};

struct list * add(struct list * l, int i) {
  struct list * new;
  new = malloc(sizeof(struct list));
  /*@ assert \valid(new); */
  new->element = i;
  new->next = l;
  return new;
}

int main() {
  struct list * l = NULL;
  l = add(l, 4);
  l = add(l, 7);
  return 0;
}
