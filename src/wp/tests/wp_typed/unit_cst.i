
/*@ 
  axiomatic C {
  logic int K1 = (int) 1 ;
  logic int K2 = (int) 2 ;
  logic integer A = 1 ;
  logic integer B = 2 ;
  } 
*/

/*@ 
  ensures A: K1 == A ;
  ensures B: K2+K1 == B+A ;
 */
void f(void) {}
