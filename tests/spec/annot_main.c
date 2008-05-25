/*@
  requires \valid(p);
  ensures *p == 0;
*/
void main(int*p) {
  *p = 0;
  CEA_DUMP();
}
