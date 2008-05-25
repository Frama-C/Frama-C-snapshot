
struct t {
  int tab[5];
};

/*@ requires \valid(i);
  @ ensures *i == 1;
  @ */
void g(int* i) {
  *i = 1;
}

/*@ requires \valid(a) && \valid_range(a->tab,0,4); */
void f(struct t* a) {
  g (&a->tab[0]);
  //@ assert a->tab[0] == 1;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make array_field_addr"
End:
*/
