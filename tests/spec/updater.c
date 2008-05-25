struct S {int a[5]; int b;} x = { 1,.b=2 } ;

/*@ axiom foo : x == {x for b = (int)(x.b+1) } ; */

/*@ axiom foo2 : x == {x for b = (int)(x.a[0]+1) } ; */

/* axiom bar : x == {x for a[4] = 0 } ; */

int * f(void) {
  return x.a;
}
