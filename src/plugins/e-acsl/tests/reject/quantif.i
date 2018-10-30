/* run.config
   COMMENT: invalid quantifications */

int main(void) {
  int z;
  /*@ assert \forall integer x; x >= 0; */
  /*@ assert \forall integer x; x == 1 ==> x >= 0; */
  /*@ assert \forall int x; 0 <= x ==> x >= 0; */
  /*@ assert \forall float x; 0 <= x <= 3 ==> x >= 0; */
  /*@ assert \forall integer x,y; 0 <= x <= 3 ==> x >= 0; */
  /*@ assert \forall integer x; 0 <= x <= 3 && 0 <= z <= 3 ==> x >= 0; */
  /*@ assert \forall integer x,y; 0 <= x <= 3 || 0 <= y <= 3 ==> x >= 0; */
  /*@ assert \forall int x; 0 <= x+1 <= 3 ==> x >= 0; */
  /*@ assert \forall integer x; 0 <= x < 10 && 9 <= x < 20 ==> x > z; */
  /*@ assert \forall integer x,z,y; 0 <= x < 2 && 0 <= y < 5 && 0 <= z <= y
    ==> x+z <= y+1; */ 
  return 0;
}
