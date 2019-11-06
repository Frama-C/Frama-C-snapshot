/* run.config
   COMMENT: bts #2305, taking the address of a bitfield
*/

#include <stdbool.h>

struct bitfields {
  int i : 2;
  bool j : 1;
} t;

int test(struct bitfields *a)
{
  return a->i;
}

int main(int argc, char **argv)
{
  //@ assert \valid_read(&(t.j));
  //@ assert \valid_read(&(t.j) + (1..3));
  t.j = 1;
  //@ assert \initialized(&(t.j));
  return test(&t);
}
