#include <stddef.h>

typedef struct stack_t {
  int *data;
  unsigned long size;
  unsigned long used;
} stack_t;

void stack_init (stack_t *, int *, unsigned long);
int stack_valid (const stack_t *stack);
int stack_push (stack_t *, int);
int stack_pop (stack_t *, int *);

/*@ predicate stack_initialized (stack_t s) =
  @ (0 < s.size) && \valid (s.data) && \valid_range (s.data, 0, s.size);
  @
  @ predicate stack_full (stack_t s) =
  @ stack_initialized (s) && (s.used == s.size);
  @
  @ predicate stack_empty (stack_t s) =
  @ stack_initialized (s) && (s.used == 0);
  @ */

/*@ requires \valid (stack);
  @ ensures 0 <= \result <= 1;
  @ ensures stack_initialized (*stack) ==> \result == 1;
  @ */

int
stack_valid (const stack_t *stack)
{
  return ((stack->data != NULL) && (stack->size != 0));
}

/*@ requires \valid (stack);
  @ requires \valid (data);
  @ requires \valid_range (data, 0, size);
  @ requires 0 < size;
  @ ensures stack_initialized (*stack);
  @ ensures stack->used == 0;
  @ */

void
stack_init (stack_t *stack, int *data, unsigned long size)
{
  stack->data = data;
  stack->size = size;
  stack->used = 0;
}

/*@ requires \valid (stack);
  @ requires stack_initialized (*stack);
  @ ensures !stack_full (\old (*stack)) ==> \old(stack->used) < stack->used;
  @ */

int
stack_push (stack_t *stack, int item)
{
  if (stack->used < stack->size) {
    stack->used++;
    stack->data [stack->used] = item;
    return 1;
  } else
    return 0;
}

/*@ requires \valid (stack);
  @ requires stack_initialized (*stack);
  @ requires \valid (item); */

int
stack_pop (stack_t *stack, int *item)
{
  if (stack->used > 0) {
    *item = stack->data [stack->used];
    stack->used--;
    return 1;
  } else
    return 0;
}

int main (void) {return 0;}
