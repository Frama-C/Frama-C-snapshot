
struct S {
  int i;
  int* p;
};

/*@ requires \valid(s);
  @ ensures *(s->p) == \old(i);
  @ */
void gs(struct S *s, int i) {
  *(s->p) = i;
}

/*@ ensures \result == 0;
  @ */
int fs() {
  struct S s;
  s.i = 0;
  s.p = &s.i;
  gs(&s, s.i);
  return s.i;
}

/*@ requires \valid(s) && \valid(*s);
  @ ensures *((*s)->p) == \old(i);
  @ */
void gps(struct S **s, int i) {
  *((*s)->p) = i;
}

/*@ ensures \result == 0;
  @ */
int fps() {
  struct S s, *ps;
  s.i = 0;
  s.p = &s.i;
  ps = &s;
  gps(&ps, s.i);
  return s.i;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make addrof_field"
End:
*/
