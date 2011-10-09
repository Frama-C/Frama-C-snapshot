/* run.config
   OPT: -simplify-cfg -keep-switch -print -check -journal-disable
   OPT: -simplify-cfg -print -check -journal-disable
*/

void f() {
  int i = 0;
  //@ loop invariant 0 <= i <= 10;
  while (i < 10) { // @ invariant 0 <= i < 10;
    ++i;
    //@ assert 0 <= i <= 10;
  }
}
