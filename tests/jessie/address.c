

void f() {
  int i, j;
  //@ assert &i != &j;
  if (&i < &j) return;
  //@ assert &i >= &j;
  if (&i - &j > 0) return;
}
