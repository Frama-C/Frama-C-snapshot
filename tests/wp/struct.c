
struct S { int i; };

/*@ ensures \forall struct S v ;  (v.i == 0 ==> \result == v);
  @ */
struct S f() {
  struct S s = { 0 };
  return s;
}

struct S2 { int a; struct S s; int t[2]; struct S ts[5]; } Gs, Ps;

/*@ ensures Gs.a == 1 && Gs.s.i == 1
         && Gs.t[0] == 3 && Gs.t[1] == 3
	 && Gs.ts[3].i == 4;
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
