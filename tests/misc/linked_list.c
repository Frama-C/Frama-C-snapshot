/* run.config
   STDOPT:
   STDOPT: +"-plevel 100" +"-big-ints-hex 257"
   STDOPT: +"-slevel 12" +"-big-ints-hex 257"
*/

#define FRAMA_C_MALLOC_HEAP
#include "share/libc/stdlib.c"
#include "share/libc/stdio.h"
#include "share/libc/stdio.c"

struct list_el {
   int val;
   struct list_el * next;
};

typedef struct list_el item;

void main() {
   item * curr, * head;
   int i;

   head = NULL;

   for(i=1;i<=10;i++) {
      CEA_DUMP();
      curr = (item *)malloc(sizeof(item));
      CEA_DUMP();
      curr->val = i;
      curr->next  = head;
      head = curr;
   }

   curr = head;

   while(curr) {
      printf("%d\n", curr->val);
      curr = curr->next ;
   }
}
