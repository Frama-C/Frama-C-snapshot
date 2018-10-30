/* run.config
   COMMENT: complex fields and indexes + potential RTE in \initialized
   STDOPT: +"-no-val-alloc-returns-null"
*/

#include "stdlib.h"

extern void *malloc(size_t p);

struct spongeStateStruct {
   unsigned char __attribute__((__aligned__(32))) state[1600 / 8] ;
   unsigned char __attribute__((__aligned__(32))) dataQueue[1536 / 8] ;
   unsigned int bitsInQueue ;
} __attribute__((__aligned__(32)));
typedef struct spongeStateStruct spongeState;

int main(void) {
  spongeState* state = (spongeState*) malloc(sizeof(spongeState));
  state->bitsInQueue = 16;

  /*@ assert
      ! \initialized(&state->dataQueue[state->bitsInQueue/(unsigned int)8]);
    */

  free(state);
  return 0;
}

