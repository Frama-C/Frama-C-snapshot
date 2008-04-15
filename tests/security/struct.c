/* run.config
   GCC:
   OPT: -security-analysis -security-lattice weak -security-propagate-assertions -journal-disable
*/

//#define GCC
#ifdef GCC
#include <stdio.h>
#include <stdlib.h>
#define INIT(f) void f(t *x, int a, int b) { *x->a = a; x->b = b; }
#else /* ! GCC */
#define FRAMA_C_MALLOC_INDIVIDUAL
#include <share/malloc.c>
#define INIT(f) void f(t *x, int a, int b);
#endif

typedef struct { int b; int *a; } t;

/*@ requires security_status(v) == public; */
void send(int v);

void send_all(const t *data) {
  send(*data->a);
  send(data->b);
  printf("a = %d; b = %d\n", *data->a, data->b);
}

void copy(t *dst, const t *src) {
  *dst->a = *src->a;
  dst->b = src->b;
}

void init(t *x, int a, int b) {
  *x->a = a;
  x->b = b;
}

/*@ assigns x->a \from a;
    ensures security_status( *x->a) == public &&
            security_status(x->b) == private;
*/
INIT(semi_public_init)

/*@ assigns x->a \from a;
    ensures security_status( *x->a) == public &&
            security_status(x->b) == public; */
INIT(public_init)

/*@ assigns x->a \from a;
    ensures security_status( *x->a) == private &&
            security_status(x->b) == private;
*/
INIT(private_init)

int main() {
  t x;
  t y;

  x.a = malloc(sizeof(int));
  y.a = malloc(sizeof(int));

  init(&x, 0, 1);
  init(&y, 2, 3);

  send_all(&x); /* double faille averee */
  send_all(&y); /* double faille averee */

  copy(&y, &x);
  send_all(&y); /* double faille averee mais deja indiquee pour x */

  semi_public_init(&x, 4, 5);
  send_all(&x); /* faille averee sur x->b */

  copy(&y, &x);
  send_all(&y); /* faille averee sur y->b mais deja indiquee pour x */

  public_init(&x, 6, 7);
  send_all(&x);

  copy(&y, &x);
  send_all(&y);

  private_init(&x, 8, 9);
  send_all(&x); /* double faille averee */

  copy(&y, &x);
  send_all(&y); /* double faille averee mais deja indiquee pour x */

  return 0;
}
