/* This is a C file */
#define FRAMA_C_MALLOC_HEAP
#include "../../share/malloc.c"

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
