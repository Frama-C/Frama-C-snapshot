int b, *p;
/*@ requires p != 0 ; // accepted (null pointer constant)
*/
int main() {
  /*@ assert p !=4 ; */ // forbidden
  p = b?4:7 ;
  /*@ assert p !=b; */ // forbidden
  return 1;
}
