/* run.config_qualif
   OPT: -wp -wp-par 1
*/

/* -------------------------------------------------------------------------- */
/* --- Testing Performance of repeated access-update                      --- */
/* -------------------------------------------------------------------------- */

struct S {
  int i;
  int j;

};

struct T {
  struct S a;
  struct S b;
} q;

/*@ ensures qed_ok: P1:\result.a.i == p.a.i && \result.a.j == p.a.j;
  @ ensures qed_ok: P2:\result.b.i == p.b.i && \result.b.j == p.b.j;
  @ */
struct T id(struct T p) {
  return p;
}


/*@
  @ assigns qed_ok: q;
  @ ensures qed_ok: P3:q.a.i == p.a.i && q.a.j == p.a.j;
  @ ensures qed_ok: P4:q.b.i == p.b.i && q.b.j == p.b.j;  
  @ ensures qed_ok: P5: q == p ;
  @ */
void g(struct T p) {
  q.a.i = p.a.i; 
  q.b =p.b;
  q.a.j = p.a.j;
}

struct R {
  int f0 ; 
  int f1 ;
  int f2 ; 
  int f3 ;
  int f4 ; 
  int f5 ;
  int f6 ; 
  int f7 ;
  int f8 ; 
  int f9 ;
  int f10 ; 
  int f11 ;
} s;

/*@
  ensures qed_ok: E0: s.f0 == 0 ; 
  ensures qed_ok: E1: s.f1 == 1 ; 
  ensures qed_ok: E2: s.f2 == 2 ; 
  ensures qed_ok: E3: s.f3 == 3 ;
  ensures qed_ok: E4: s.f4 == 4 ; 
  ensures qed_ok: E5: s.f5 == 5 ; 
  ensures qed_ok: E6: s.f6 == 6 ; 
  ensures qed_ok: E7: s.f7 == 7 ;
  ensures qed_ok: E8: s.f8 == 8 ; 
  ensures qed_ok: E9: s.f9 == 9 ; 
  ensures qed_ok: E10: s.f10 == 10 ; 
  ensures qed_ok: E11: s.f11 == 11 ;
 */
void f (void) 
{
  s.f0 = 0 ; 
  s.f1 = 1 ; 
  s.f2 = 2 ; 
  s.f3 = 3 ;
  s.f4 = 4 ; 
  s.f5 = 5 ; 
  s.f6 = 6 ; 
  s.f7 = 7 ;
  s.f8 = 8 ; 
  s.f9 = 9 ; 
  s.f10 = 10 ; 
  s.f11 = 11 ;
}

