/* run.config
   OPT: -load-script tests/spec/bts0655.ml
*/
/*@
  @ ensures \result == \max( a, b );
  @ ensures \result != \min( a, b );
  @ ensures \max(a,b) != \min(a+1,b);
  @ ensures a == \abs( a );
  @*/
unsigned int
max( unsigned int a, unsigned int b )
{
    int i = a > b ? a : b;
    //@ assert i == \max( \at(a,Pre), \at(b,Pre) );
    return i;
}
