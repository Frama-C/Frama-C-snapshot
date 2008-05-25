
struct S {
  int i;
  int* p;
};

/*@ requires \valid(s) && \valid(s->p);
  @ ensures s->i == 1;
  @ ensures *s->p == 2;
  @ */
void f(struct S *s) {
  int *tmp = s->p;
  s->p = &s->i;
  s->i = 2;
  *tmp = *s->p;
  s->p = tmp;
  s->i = 1;
}

struct T {
  int ti;
  int tj;
};

/*@ requires \valid(t);
  @ ensures t->ti == 1;
  @ ensures t->tj == 2;
  @ */
void fc(struct T *t) {
  struct T tmp = { 1, 2 };
  *t = tmp;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make compound_local"
End:
*/
