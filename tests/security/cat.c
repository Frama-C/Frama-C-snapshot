/* run.config
   GCC:
   OPT: -security-analysis -lib-entry f -security-lattice weak
   OPT: -security-analysis -lib-entry f -security-lattice strong
   */

// #define GCC
#ifdef GCC
  #include <stdlib.h>
#else
  #define FRAMA_C_MALLOC_INDIVIDUAL
  #include <share/malloc.c>
#endif

/*@ requires security_status(x) == public(); */
void send(int x);

/*@ ensures security_status( *x) == public(); */
void crypt(int* x);

/*@ ensures security_status( *x) == private(); */
void uncrypt(int* x);

int c;

int f() {
  int x = (int /*@ public */) 0;
  int y = 2, z = x; /* z public */
  int t = y;        /* x prive  */

  crypt(&y);        /* y public */
  if (z) uncrypt(&y);
  /* y public car on sait que z == 0 */
  send(z);
  send(y);
  send(t); /* faille averee      */

  if (c) uncrypt(&y);
  /* y approxime */
  send(y); /* faille potentielle */

  if (y) z = z + (int /*@ public */) 1;
  send(z); /* faille potentielle si dep de ctrl */

  return 0;
}
