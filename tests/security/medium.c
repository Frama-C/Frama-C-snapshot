/* run.config
   GCC:
   OPT: -security-analysis -security-lattice weak -lib-entry f
   OPT: -security-analysis -security-lattice medium -lib-entry f
   OPT: -security-analysis -security-lattice strong -lib-entry f
   */

#define FRAMA_C_MALLOC_INDIVIDUAL
#include <share/malloc.c>

/*@ requires security_status(y) == public() ;
    ensures security_status(y) == concrete(); */
void send(const int y);

/*@ ensures
      security_status( *z) == public() &&
      security_status( *z) == abstract(); */
void crypt(int* z);

int y;

/* \result cannot be used here. See bug #121 */
/*@ ensures security_status( \result) == abstract(); */
int read_key();

int f() {
  int x;

  x = (y) ? (int /*@ public */) 1 : (int /*@ public */) 2;
  send(x); // emission non securisee si dep de ctrl (lattice medium ou strong)

  x = (read_key()) ? (int /*@ public */) 1 : (int /*@ public */) 2;
  send(x); // emission non securisee avec lattice strong

  int y = read_key();
  x = (y) ? 3 : 4;
  send((int /*@ public */) y); // emission non securisee avec lattice strong

  return 0;
}
