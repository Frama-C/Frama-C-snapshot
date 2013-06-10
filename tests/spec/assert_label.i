/* run.config
STDOPT: +"-copy"
*/

void bar () { 
//@ assert bli: \true;
}

void f() {
 L: //@ assert lab: \true;
  ;
}

void foo (int n) {
  switch (n) {
  case 4:
    /*@ assert "foo + bar" "="
      @        "foobar": \true; */
    break;
  case 5:
    //@ assert foo: \true;
    break;
  case 6:
    //@ assert bar: \true;
  case 7:
    //@ assert bla: \true;
    ;
  }
}
