// All annotations below present an error that should be expressed by a
// friendlier msg than the usual unexpected 'bla' token

/*@ requires x >= 0
  ensures \result == 0; */
int f(int x);

/*@ ensures \result >= 0 */
int g(int x);

 
