#pragma FloatModel(strict)


/*@ logic real exp(real x) */


/*@ requires |x| <= 2^^(-3)
  @ ensures
  @ \model(\result)==exp(\model(x))
  @ && (\round_error(x)==0 => \round_error(\result)<= 2^^(-52))
  @ && \total_error(\result)<=\total_error(x)+2^^(-51)
*/

double monexp(double x) {
  double y=1+x*(1+x/2);
  //*@ \set_model y exp(\model(x)) */
  return y;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make test4_floats"
                 "LC_ALL=C make test4_floats.coq"
                 
End:
*/
