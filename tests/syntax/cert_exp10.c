/* run.config
   COMMENT: unspecified sequences
   STDOPT: +"-kernel-warn-key=CERT:EXP:10"
*/
extern int f(int);

int main(void) {
  int i = 3 ;
  int z = 3 ;
  int m1 = f(1);
  int m2 = f(2) + f(3);

  z = ( i > 0 ) ? ++i : i;

  return 0;
}
