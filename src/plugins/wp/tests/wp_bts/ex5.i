/* run.config_qualif
   DONTRUN:
*/
void dummy()
{
  int d = 7;
  //@ assert \exists integer r1, r2, r3, r4, r5; d == r1 + r2 + r3 + r4 + r5;
  int e = 7;
  //@ assert \exists integer i4, i3, i2, i1, i; e <= i + i1 + i2 + i3 + i4;
  int a = 7;
  //@ assert \exists integer i5, x; a == x + i5 && x <= i5;
}

/*@ axiomatic A {
    predicate P(integer x) reads \nothing;
    predicate Q(integer x) reads \nothing ;
    logic integer f(integer x) reads \nothing;

    predicate B(boolean x) reads \nothing;
    predicate C(boolean x,boolean y) reads \nothing ;
    logic boolean c(boolean x) reads \nothing;
    }
*/

/*@ ensures ok11: \exists integer x;                           x == f(b);
    ensures ok12: \exists integer x; \exists integer y;        x == f(y);
    ensures ok21: \exists integer x;                         1+x == f(b);
    ensures ok22: \exists integer x; \exists integer y;      1+x == f(y);
    ensures ok31: \exists integer x;                    a+x+f(a) == b+f(b);
    ensures ok32: \exists integer x; \exists integer y; a+x+f(a) == b+f(y);

    ensures ko0: \exists integer x;        x != 1;
    ensures ko1: \exists integer x;        x ==   f(x);
    ensures ko2: \exists integer x;      1+x == b+f(x);
    ensures ko3: \exists integer x;   x+f(x) == b;

    ensures ko4: \exists integer x; P(x) && \exists integer y;  x == f(y);
    ensures ko5: \exists integer x; P(x) || Q(x) ||        x == 1;

    ensures p1:  \exists integer x; P(x) && Q(x) &&        x == 1;
    ensures p2:  \exists integer x; P(x) && Q(x) &&      1+x == b;
    ensures p3:  \exists integer x; P(x) && Q(x) && 1+x+f(a) == b+f(b);

    ensures ok41: \exists boolean x; x && c(x) == c(\true);
    ensures ok42: \exists boolean x; !x && c(x) == c(\false);
    ensures ok43: \exists boolean x; \exists boolean y; !x && y && (C(x,y) <==> C(\false,\true)) ;
    ensures ko43: \exists boolean x; \exists boolean y; !x && y && C(x,y) ;
 */
void exists (int a, int b) {
}


/*@ ensures ok11: \forall integer x;                           x != f(b);
    ensures ok12: \forall integer x; \forall integer y;        x != f(y);
    ensures ok21: \forall integer x;                         1+x != f(b);
    ensures ok22: \forall integer x; \forall integer y;      1+x != f(y);
    ensures ok31: \forall integer x;                    a+x+f(a) != b+f(b);
    ensures ok32: \forall integer x; \forall integer y; a+x+f(a) != b+f(y);

    ensures ko0: \forall integer x;        x ==   f(x);
    ensures ko1: \forall integer x;        x !=   f(x);
    ensures ko2: \forall integer x;      1+x != b+f(x);
    ensures ko3: \forall integer x;   x+f(x) != b;

    ensures ko4: \forall integer x; P(x) || \forall integer y;  x != f(y);
    ensures ko5: \forall integer x; P(x) && Q(x) &&        x != 1;

    ensures p1:  \forall integer x; P(x) || Q(x) ||        x != 1;
    ensures p2:  \forall integer x; P(x) || Q(x) ||      1+x != b;
    ensures p3:  \forall integer x; P(x) || Q(x) || 1+x+f(a) != b+f(b);

    ensures q1:  \forall integer x; P(x) && Q(x) ==>        x != 1;
    ensures q2:  \forall integer x; P(x) && Q(x) ==>      1+x != b;
    ensures q3:  \forall integer x; P(x) && Q(x) ==> 1+x+f(a) != b+f(b);

    ensures r1:  \forall integer x; P(x) &&         x == 1      ==> Q(x);
    ensures r2:  \forall integer x; P(x) &&       1+x == b      ==> Q(x);
    ensures r3:  \forall integer x; P(x) &&  1+x+f(a) == b+f(b) ==> Q(x);
 */
void forall (int a, int b) {
}
