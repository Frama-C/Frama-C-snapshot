/* run.config
   GCC:
   OPT: -security-analysis -security-lattice weak
   OPT: -security-analysis -security-lattice strong
*/

//#define GCC
#ifdef GCC
#include <stdio.h>
#include <stdlib.h>
#else /* ! GCC */
#define FRAMA_C_MALLOC_INDIVIDUAL
#include <share/malloc.c>
#endif

typedef struct t_ { int a; int b; } t;
typedef struct u_ { int a; int *b; t *c; struct u_ *d; } u;

/*@ requires security_status(v) == public; */
void send(int v);

void send_t(const t *a) {
  send(a->a);
  send(a->b);
}

void send_u(const u *b) {
  t *c = b->c;
  u *d = b->d;
  send(b->a);
  send(*(b->b));
  if ((int /*@ public */)(c != NULL)) send_t(c);
/*   if (d != NULL) send_u(d); */
// inline tant que les appels recursifs ne fonctionnent pas
  if ((int /*@ public */)(d != NULL)) {
    b = d;
    c = b->c;
    d = b->d;
    send(b->a);
    send(*(b->b));
    if ((int /*@ public */)(c != NULL)) send_t(c);
    if ((int /*@ public */)(d != NULL)) {
      b = d;
      c = b->c;
      d = b->d;
      send(b->a);
      send(*(b->b));
      if ((int /*@ public */)(c != NULL)) send_t(c);
    }
  }
}

int main() {
  t x, y;
  u z, t;

  x.a = (int /*@public */) 0;
  x.b = 1;

  y.a = (int /*@public */) 2;
  y.b = 3;

  z.a = (int /*@public */) 4;
  z.b = malloc(sizeof(int));
  *z.b = 5;
  z.c = &x;
  z.d = NULL;

  t.a = (int /*@public */) 5;
  t.b = z.b;
  t.c = &y;
  t.d = &z;

  send_t(&x); /* une faille averee sur x.b */
  send_t(&y); /* une faille averee sur y.b */
  send_u(&z); /* deux faille averee sur *z.b et *(z.c).b */
  send_u(&t); /* quatre faille averee sur *t.b, *(t.c).b, (*t.d).b
		 et *(t.c).b */

  return 0;
}
