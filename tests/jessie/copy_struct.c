
struct S {
  int i;
  int j;

};

struct T {
  struct S a;
  struct S b;
};

/*@ requires \valid(p) && \valid(q);
  @ ensures p->a.i == q->a.i && p->a.j == q->a.j;
  @ ensures p->b.i == q->b.i && p->b.j == q->b.j;
  @ */
void f(struct T* p, struct T* q) {
  *q = *p;
}

/*@ ensures \result.a.i == p.a.i && \result.a.j == p.a.j;
  @ ensures \result.b.i == p.b.i && \result.b.j == p.b.j;
  @ */
struct T id(struct T p) {
  return p;
}

/*@ requires \valid(p);
  @ ensures \result.a.i == p->a.i && \result.a.j == p->a.j;
  @ ensures \result.b.i == p->b.i && \result.b.j == p->b.j;
  @ */
struct T idp(struct T* p) {
  return *p;
}

/*@ requires \valid(p);
  @ */
void g(struct T* p) {
  struct T copy = id(*p);
  struct T copy2 = idp(p);
  copy2.a.i = 0;
  /*@ assert p->a.i == copy.a.i; */
  copy.a.j = 1;
  /*@ assert p->a.j == copy2.a.j; */
  p->b.i = 0;
  /*@ assert copy.b.i == copy2.b.i; */
  /*@ assert p->b.j == copy.b.j == copy2.b.j; */
}

/* 
Local Variables:
compile-command: "LC_ALL=C make copy_struct"
End:
*/
