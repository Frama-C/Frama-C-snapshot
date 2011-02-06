typedef int a;
struct S {int v ; int a[5]; int b;} x = { 1,.b=2 } ;

struct SS {struct S a[5]; struct S b;} y;

/*@ logic struct S foo(struct S x) = {x \with .b = (int)(x.b+1), .v = (int)3 } ; */

/*@ lemma foo2 : x == {foo(x) \with .b = (int)(x.a[0]+1) } ; */

/*@ lemma bar : {foo(x) \with .a = { \with [..] = (int)0, [3] = (int)3 }} == {foo(x) \with .a[..] = (int)0, .a[3]= (int)3 } ; */

/*@ lemma bar2 : x == {x \with .a = {x.a \with [4] = (int)0 }} ; */

/*@ lemma bar3 : y == {y \with .a[3+1].b = (int)(x.b+1)} ; */

/*@ lemma bar4 : y == {y \with .a[4].a[..] = (int)(x.b+1)} ; */

/*@ lemma bar5 : y == {y \with .a[4] = {\with .a[..] = (int)(x.b+1), .v = (int)3}, .b.v = (int) 4} ; */

/*@ lemma cast : y.a[0].v == ((struct S) y).v ; */


int * f(void) {
  if (y.a[0].v == ((struct SS) y).b.v)
    return y.a[0].v ;
  return x.b ;
}

