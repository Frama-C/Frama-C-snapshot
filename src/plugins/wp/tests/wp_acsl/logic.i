/* run.config
   OPT: -wp-model Typed
*/
/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-steps 50
*/
// Test logic types defined from C types
//--------------------------------------
typedef struct { int x ; int y ; } Point ;
typedef Point Triangle[3] ;
/*@ axiomatic A {
    type triangle = Point[3];
    predicate P(triangle t) = t[1].x==20 ;
    } */

//@ assigns \nothing; ensures P((triangle)q);
void f(Point *q);

Point t[3];

//@ assigns \nothing; ensures P(t);
void h(void) {
  f(t) ;
}
// Test logic constants
//---------------------
int x = 1 ;
//@ logic integer k = 1 ;

//------------------------------------------
Triangle tr={10,11,20,21,30,31};
Triangle * q = &tr;
Point * p = &tr[0];

typedef struct { int tab[6] ; } Tint6;
Tint6 tr6 ={10,11,20,21,30,31} ;

Point pt1 = {10,11};
typedef struct { int tab[2] ; } Tint2;
Tint2 pt2 = {10,11};

typedef struct { unsigned char bytes[sizeof(unsigned)] ; } Buint;
Buint buint = { 1, 2, 4, 8 };
unsigned ui = 134480385;
/*@ requires qed_ok: x == k;
  @ requires qed_ok: P(tr);
  @ requires qed_ok: P(*q);
  @ requires qed_ok: P(*(Triangle *)p);
  @ requires qed_ok: pt1.y == ((Point) pt2).y;
  @ requires qed_ok: pt2.tab[1] == ((Tint2) pt1).tab[1];
  @ requires qed_ok: pt1.y == ((Point) pt2.tab).y;
  @ requires qed_ok: pt2.tab[1] == ((int[2]) pt1)[1];
  @ requires qed_ok: pt2.tab[1] == ((int[2]) pt2)[1];
  @ requires qed_ok: ui == (unsigned) buint;
  @ requires qed_ok: buint == (Buint) ui;
  @ requires qed_ok: tr[1].y == ((Triangle) tr6)[1].y;
  @ requires qed_ok: tr[1].y == ((Triangle) tr6.tab)[1].y;
  @ requires qed_ok: tr6.tab[4] == ((int[6])tr6)[4];
  @ requires qed_ok: pt2.tab[1] == ((int[2])tr6)[1];
  @ requires qed_ok: pt2.tab[1] == ((int[2])tr6.tab)[1];
  @ requires qed_ok: pt2.tab[1] == ((Tint2)tr6).tab[1];
  @ requires qed_ok: pt2.tab[1] == ((Tint2)tr6.tab).tab[1];
 */
int main() {
  return *(unsigned *)&buint;
}

//--------------------------------------

/*@ axiomatic B {
  @   type Aint2 = int[2];
  @   type Aint6 = int[6];
  @   type Aint2x3 = Aint2[3];
  @   logic Point pt reads \nothing;
  @   logic Aint2 a2 reads \nothing;
  @   logic Aint6 a6 reads \nothing;
  @   logic Tint2 s2 reads \nothing;
  @   logic Point[3] p3 reads \nothing;
  @   logic Point[2] p2 reads \nothing;
  @   logic Aint2x3 a2x3 reads \nothing;
  @ } */

// todo: lemma trunc1:  (Point)p3 == p3[0];
// todo: lemma trunc2:  (Point[2])p3 == p2 <==> p2[0]==p3[0] && p2[1]==p3[1];
// todo: lemma trunc3: (Aint2)a2x3 == a2x3[0];
// todo: lemma trunc4: (Aint2)a6 == ((Aint2x3)a6)[0];

// todo: lemma extend1: ((Point[3])pt)[0] == pt;
// todo: lemma extend2: (Point[3])p2 == p3  ==> p2[0]==p3[0] && p2[1]==p3[1];

// todo: lemma fits_eq1: (Point)a2 == pt <==> pt.x==a2[0] && pt.y==a2[1];
// todo: lemma fits_eq2: (Aint2)pt == a2 <==> pt.x==a2[0] && pt.y==a2[1];
// todo: lemma fits_eq3: (Aint2)s2 == s2.tab ;
// todo: lemma fits_eq4: (Point)s2 == pt <==> pt.x==s2.tab[0] && pt.y==s2.tab[1];
// todo: lemma fits_eq5: (Aint2)(p3[2]) == ((Aint2x3)p3)[2];
// todo: lemma fits_eq6: ((Aint6)p3)[5] == ((Aint2x3)p3)[2][1];

// todo: lemma fits_trunc1:  (int)pt == pt.x;
// todo: lemma fits_trunc2:  (Aint2)p3 == a2 <==> p3[0].x==a2[0] && p3[0].y==a2[1];

//--------------------------------------
