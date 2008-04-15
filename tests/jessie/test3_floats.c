#pragma FloatModel(strict)


/*@ requires y/2.0 <= x <= 2.0*y; 
  @ ensures \result == x-y;
  @*/

float Sterbenz(float x, float y) {
  return x-y;
}

/*@ requires 2.0 == x;
  @ ensures \result == 1.0;
  @*/

float f(float x) { 
  return Sterbenz(x,Sterbenz(x,1.f));
}




/* 
Local Variables:
compile-command: "LC_ALL=C make test3_floats"
                 "LC_ALL=C make test3_floats.coq"
                 
End:
*/
