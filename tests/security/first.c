/* run.config
   GCC:
   OPT: -security-analysis -security-lattice weak -journal-disable
   */

#define FRAMA_C_MALLOC_INDIVIDUAL
#include <share/malloc.c>

/*@ requires security_status(y) == public; */
void send(const int y);

/*@ ensures security_status( *z) == public; */
void crypt(int* z);

int main() {
  int x;
  int a;

  x=0;
  crypt(&x);
  send(x); // emission securisee

  int *b = malloc(sizeof(int));
  int *c = malloc(sizeof(int));

  a = (int /*@ public */) 0;    // status(a) = public
  c = b;    // status(c) = prive
  a = *c;   // status(a) = prive
  crypt(c); // status(*c) = public (donc status(*b))
  x = *b ;  // status(x) = status(*b) = public

  send(0);  // emission non securisee : faille detectee
  send(x);  // emission securisee
  send(a);  // emission non securisee : faille detectee
  send(*c); // emission securisee
  send(*b); // emission securisee

  return 0;
}
