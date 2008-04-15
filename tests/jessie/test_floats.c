#pragma FloatModel(strict)

//@ ensures \result == 7.0;
double test1() {
  return 2.0 * 3.5;
}

//@ ensures \result == 0.100000001490116119384765625;
float test2() { 
  return 0.1f;
}




/* 
Local Variables:
compile-command: "LC_ALL=C make test_floats"
                 "LC_ALL=C make test_floats.coq"
                 
End:
*/


