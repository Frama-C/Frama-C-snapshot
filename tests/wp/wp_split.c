/* run.config
   OPT: -wp -wp-model Hoare -wp-split -wp-print -wp-split-dim 2
*/
int a, b, c, d, e;

/*@ 
  assigns \nothing;

  behavior split_and:
    assumes    a==0 && b==0 && c==0 && d==0 && e==0;
    ensures ok:a==0 && b==0 && c==0 && d==0 && e==0;

  behavior split_if:
    assumes    (a==0 ==> b==0) && (a!=0 ==> c==0);
    ensures ok:(a==0 ? b==0 : c==0) ;

  behavior split_forall:
    assumes    (\forall integer x ; (0<x<a ==>  x<b)) && c==0;
    ensures ok: \forall integer x ; (0<x<a ==> (x<b   && c==0)) ;

  behavior split_not:
    assumes    !a==0;
    ensures ok:!a==0;

  behavior split_not_not:
    assumes    a==0 && b==0;
    ensures ok:!(!a==0 || !b==0);

  behavior split_not_or:
    assumes    !(a==0 || b==0);
    ensures ok:!(a==0 || b==0);

  behavior split_not_if:
    assumes     (a==0 ==> b!=0) && (a!=0 ==> c!=0);
    ensures ok:!(a==0 ? b==0 : c==0) ;

  behavior split_not_exists:
    assumes     (\forall integer x ; (0<x<a ==>  x<b)) && c==0;
    ensures ok: !\exists integer x ;(0<x<a && (!(x<b)  || c!=0) ) ;

 */
void f(void) {

}

