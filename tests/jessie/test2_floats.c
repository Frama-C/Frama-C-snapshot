#pragma FloatModel(strict)


/*@ axiomatic Sqrt {
  @   logic real real_sqrt(real x);
  @ }
  @*/


/*@ ensures \result == (double)(real_sqrt(x)); 
  @*/
double sqrt(double x);

/*@ ensures \result == 0.0; 
  @*/
double poly() {
  double x = sqrt(2.0)/2.0;
  return 2.0*x*x-1.0;
}



/* 
Local Variables:
compile-command: "LC_ALL=C make test2_floats"
                 "LC_ALL=C make test2_floats.coq"
                 
End:
*/


