#pragma FloatModel(strict)


/*@ axiomatic axioma {
  @ logic real powerRZ(real x, integer n);
  @ logic integer R2Z(real x);
  @ }
  @*/

void main() {
  
  float x = 1.0f, y = 0.0f;
  
  /*@ loop invariant x-y == 1.0;
    @ loop assigns x,y;
    @ loop variant (16777216-R2Z(y));  
    @*/
  while (y != x) {y = x ; x += 1.0f;}
  //@ assert x == y == powerRZ(2.0,24);
}



/* 
Local Variables:
compile-command: "LC_ALL=C make test5_floats"
                 "LC_ALL=C make test5_floats.coq"
                 
End:
*/
