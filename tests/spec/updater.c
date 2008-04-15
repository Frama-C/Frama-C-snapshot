struct S {int a[5]; int b;} x = { 1,.b=2 } ;

/*@ lemma foo : x == {x for b = (int)(x.b+1) } ; */

/*@ lemma foo2 : x == {x for b = (int)(x.a[0]+1) } ; */

/* lemma bar : x == {x for a[4] = 0 } ; */

int * f(void) {
  return x.a;
}
