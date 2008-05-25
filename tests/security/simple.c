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

/*@ requires security_status(y) == public(); */
void send(int y);

/*@ ensures security_status( *z) == public(); */
void crypt(int* z);

int t;
int x;
int a;
int *b, *c;

int f() {
  b = malloc(sizeof(int));
  c = malloc(sizeof(int));
  a = (int /*@ public */) 0;  /* status(a) = public */
  if ((int /*@ public */) (b != NULL && c != NULL)) {
    *b = 2; /* status(*b) = prive */
    if (t>0) {
      *b = a+t; /* status(*b) = status(a+t) = approxime */
      crypt(b);  /* status(*b) = public */
      x = *b;    /* status(x) = public */
    } else {
      c = b;    /* status(c) = prive */
      a = *c;   /* status(a) = prive */
      crypt(c); /* status(*c) = public (donc status(*b) aussi) */
      x = *b;   /* status(x) = status(*b) = public */
    }
    /* status(x)=status(*b)=public mais deduction potentielle d'infos sur t */
    /* status(a) = approxime (cause : affectation a=*c) */
    /* status(*c) = approxime (cause : initialisation) */

    send((int /*@ public */)0);
      /* emission securisee mais dep envers conditionnelle */

    send(x);  /* emission securisee mais permet d'observer t,
		 donc alarme si dep de ctrl */
    send(a);  /* emission non securisee : alarme declenchee */
    send(*c); /* emission non securisee : alarme declenchee */
    send(t);  /* emission non securisee : faille toujours declenchee 
		 (car pas de dependance envers tests <> NULL: 
		 b et c ne peuvent être NULL. */
  }
  return 0;
}
