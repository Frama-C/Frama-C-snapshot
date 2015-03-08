/* run.config
   OPT: -load-script tests/report/one_hyp.ml
   OPT: -load-script tests/report/several_hyps.ml
*/

void f();
void f2();

void g() {
  /*@ assert \true; */
}

void h() {
  /*@ assert \false; */
}

void i() {
  /*@ assert 1 == 2; */
}

void j() {
  /*@ assert 2 == 3; */
}

void main() {
  /*@ assert 0 == 1; */
  f();
  f2();
  g();
  h();
  i();
  j();
}
