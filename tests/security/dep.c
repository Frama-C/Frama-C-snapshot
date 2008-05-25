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
  #include "share/malloc.c"
#endif

/*@ requires security_status(x) == public(); */
void send(int x);

/*@ ensures security_status( *x) == public(); */
void crypt(int* x);

int a, b, c, d;

void g(int x) { send(x); }

void f() {
  a = 0;
  b = 0;
  crypt(&a);
  crypt(&b);
  send(a);

  if (c) send(a);  /* faille potentielle si dep de ctrl */
  if (!c) g(a);    /* faille potentielle si dep de ctrl */

  if (c)
    g(a);    /* failles potentielles si dep de ctrl */
  else
    send(b); /* failles potentielles si dep de ctrl */

  c = 0; crypt(&c);
  send(c);

  if (d) { d = 1; crypt(&d); } else { d = 2; crypt(&d); }
  send(d);   /* faille potentielle si dep de ctrl */
}
