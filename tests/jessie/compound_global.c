
struct T {
  int ti;
  int tj;
};

struct T gt = { 1, 2 };

/*@ global invariant gt:
  @   gt.ti == 1 && gt.tj == 2;
  @ */

/*@ requires \valid(t);
  @ ensures t->ti == 1;
  @ ensures t->tj == 2;
  @ */
void fc(struct T *t) {
  *t = gt;
}

struct S {
  int i;
  int* p;
};

struct S gs = { 1 };

/*@ global invariant gt2:
  @   gs.i == 1 && gs.p == \null;
  @ */

/*@ requires \valid(s);
  @ ensures s->i == 1;
  @ ensures s->p == \null;
  @ */
void fcp(struct S *s) {
  *s = gs;
}

/*
Local Variables:
compile-command: "LC_ALL=C make compound_global"
End:
*/
