/* run.config
CMD: FRAMAC_SHARE=./share ./bin/toplevel.opt
OPT: -no-lib-entry -wp -wp-model Store  -wp-proof none -wp-print -wp-verbose 2 -wp-assigns effect
OPT: -no-lib-entry -wp -wp-model Hoare  -wp-proof none -wp-print -wp-verbose 2
OPT: -no-lib-entry -wp -wp-model UnsafeCaveat -wp-proof none -wp-print -wp-verbose 2
OPT: -no-lib-entry -wp -wp-model Runtime -wp-proof none -wp-print -wp-verbose 2 -wp-assigns effect
FILTER: tests/wp/filter_wp_res
*/

/* run.config_why
   OPT: -wp -wp-assigns effect -wp-timeout 1 -wp-proof alt-ergo -wp-par 1
 */

struct S { int i; };

/*@ ensures p1: oracle_ok: \forall struct S v ;  (v.i == 0 ==> \result == v);
    ensures p2: oracle_ok: \forall struct S v ;  \result == { v \with .i = (int)0 } ;
    assigns \nothing ;
  @ */
struct S f(void) {
  struct S s = { 0 };
  return s;
}

struct S2 { int a; struct S s; int t[2]; struct S ts[5]; } Gs, Ps;
struct S Gs1, Gs2;
/*@ensures p1: oracle_ok: Gs.a == 1 ;
   ensures p2: oracle_ok: Gs.s.i == 1 ;
   ensures p3: oracle_ok: Gs.t[0] == 3 && Gs.t[1] == 3 ;
   ensures p4: oracle_ok: Gs.ts[3].i == 4;

   ensures p5: oracle_ok: Gs1 == {\old(Gs1) \with .i = (int) 1 } ;
   ensures p6: oracle_ok: Gs.s == {\old(Gs.s) \with .i = (int) 1 } ;

   // assigns Gs.a, Gs.s.i, Gs.t[0], Gs.t[1], Gs.ts[3].i;  <----- FAUX
   assigns Gs.a, Gs.s.i, Gs.t[0], Gs.t[1], Gs.ts[3].i, Gs1.i, Gs2.i ;
  @ */
void f2 () {
  Gs.a = 1;
  Gs.s.i = Gs.a;
  Gs.t[0] = 3;
  Gs.t[Gs.a] = 3;
  Gs.ts[3].i = Gs.t[0] + 1;
  Gs1.i = 1;
  Gs2.i = 1;
  //@ assert a1: SI_PAS_DE_PADDING: oracle_ok: Gs1==Gs2; // <--- should be prouvable
}

union U { int a; int b; char c; } Gu;

void fu () {
  Gu.a = 0;
  //@ assert a1: oracle_ok: Gu.a == 0;
  Gu.b = 1;
  //@ assert a2: oracle_ok: Gu.b == 1;
}
union U Ggu; 
void fu2 () {
  Gu.a = 0;
  Gu.b = 1;
  //@ assert a1: oracle_ok: Gu.a == 1; 
  // this one is ok but need M3 (or maybe M2 ???)
  //@ assert a2: oracle_ko: Gu.a == 0; 
  // the last one is false !
  Gu.c = 2;
  Ggu.a=0;
  Ggu.b=1; 
  Ggu.c = 2;
  //@ assert a3: SI_PAS_PADDING: oracle_ok: Ggu==Gu; // <--- should be prouvable 
}
int main (void) { return 0 ; }

