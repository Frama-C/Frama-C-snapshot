/* run.config
   COMMENT: typing bug of Issue 69
*/

int main(void) {
  /*@ assert \forall unsigned char c; 4 <= c <= 300 ==> 0 <= c <= 255; */ ;

  int n = 5;
  /*@ assert
        \let m = n > 0 ? 4 : 341;
        \forall char u; 1 < u < m ==> u > 0; */ ;
}
