/* run.config
   STDOPT: +"-simplify-cfg" +"-keep-switch"
   STDOPT: +"-simplify-cfg"
*/

void f() {
  int i = 0;
  //@ loop invariant 0 <= i <= 10;
  while (i < 10) { // @ invariant 0 <= i < 10;
    ++i;
    //@ assert 0 <= i <= 10;
  }
}
