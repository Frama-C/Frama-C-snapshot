/* run.config*
   STDOPT: #"-load-module variadic -no-val-builtins-auto"
   STDOPT: #"-load-module variadic -plevel 100 -big-ints-hex 257 -no-val-builtins-auto"
   STDOPT: #"-load-module variadic -slevel 12 -big-ints-hex 257 -no-val-builtins-auto"
*/

#include "__fc_define_size_t.h"
#include "__fc_define_null.h"

/* Size of mallocable memory in bytes. */
#define MEMORY_SIZE (1<<10)

char MEMORY[MEMORY_SIZE];

void *malloc(size_t size) {
  static int next_free = 0;
  next_free += size;
  if (next_free>=MEMORY_SIZE) return NULL;
  return (MEMORY+(next_free-size));
}

#include "stdio.h"
#include "stdio.c"


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
      Frama_C_dump_each();
      curr = (item *)malloc(sizeof(item));
      Frama_C_dump_each();
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
