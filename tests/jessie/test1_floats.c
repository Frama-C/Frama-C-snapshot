#pragma FloatModel(strict)



/*@ axiomatic axo {
  @ logic real cos(real x); 
  @ logic real power(real x, integer n);
  @ logic real abs(real x);  
  @ }
  @ */



/*@ requires abs(x) <= 1.0/32.0;
  @ ensures abs(\result - cos(x)) <= power(2.0,-23);
  @ */

float moncos(float x) {
  return 1.0f-x*x*0.5f;
}



/* 
Local Variables:
compile-command: "LC_ALL=C make test1_floats"
                 "LC_ALL=C make test1_floats.coq"
                 
End:
*/


