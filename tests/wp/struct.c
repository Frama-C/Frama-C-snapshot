
struct S { int i; };

/*@ ensures \forall struct S v ;  (v.i == 0 ==> \result == v);
    assigns \nothing ;
  @ */
struct S f(void) {
  struct S s = { 0 };
  return s;
}

struct S2 { int a; struct S s; int t[2]; struct S ts[5]; } Gs, Ps;

/*@ 
   ensures Gs.a == 1 && Gs.s.i == 1
         && Gs.t[0] == 3 && Gs.t[1] == 3
	 && Gs.ts[3].i == 4;
    assigns Gs.a, Gs.s.i, Gs.t[0], Gs.t[1], Gs.ts[3].i;
  @ */
void f2 () {
  Gs.a = 1;
  Gs.s.i = Gs.a;
  Gs.t[0] = 3;
  Gs.t[Gs.a] = 3;
  Gs.ts[3].i = Gs.t[0] + 1;
}

void main (void) {
  f();
  f2();
}

union U { int a; int b; char c; } Gu;

void fu () {
  Gu.a = 0;
  //@ assert Gu.a == 0;
  Gu.b = 1;
  //@ assert Gu.b == 1;
}
void fu2 () {
  Gu.a = 0;
  Gu.b = 1;
  //@ assert Gu.a == 1; 
  // this one is ok but need M3 (or maybe M2 ???)
  //@ assert Gu.a == 0; 
  // the last one is false !
}
