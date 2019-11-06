/* run.config
   COMMENT: typedef (from a Bernard's bug report)
*/

typedef unsigned char uint8;

int main(void) {
  uint8 x = 0;
  /*@ assert x == 0; */ ;
  return 0;
}
