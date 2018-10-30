/* run.config
   COMMENT: quantifiers
*/

int main(void) {

  // simple universal quantifications

  /*@ assert \forall integer x; 0 <= x <= 1 ==> x == 0 || x == 1; */
  /*@ assert \forall integer x; 0 < x <= 1 ==> x == 1; */
  /*@ assert \forall integer x; 0 < x < 1 ==> \false; */
  /*@ assert \forall integer x; 0 <= x < 1 ==> x == 0; */

  /* // multiple universal quantifications */

  /*@ assert \forall integer x,y,z; 0 <= x < 2 && 0 <= y < 5 && 0 <= z <= y
    ==> x+z <= y+1; */

  // simple existential quantification

  /*@ assert \exists int x; 0 <= x < 10 && x == 5; */

  // mixed universal and existential quantifications

  /*@ assert \forall int x; 0 <= x < 10
    ==> x % 2 == 0 ==> \exists integer y; 0 <= y <= x/2 && x == 2 * y; */

  { // Gitlab issue #42
    int buf[10];
    unsigned long len = 9;
    /*@ assert \forall integer i; 0 <= i < 10 ==> \valid(buf+i); */
    /*@ assert \forall char i; 0 <= i < 10 ==> \valid(buf+i); */
    /*@ assert \forall integer i; 0 <= i < len ==> \valid(buf+i); */
    /*@ assert \forall integer i; 0 <= i <= len ==> \valid(buf+i); */
  }

  return 0;
}
