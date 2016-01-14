/*@
  requires \valid(p);
  ensures *p == 0;
*/
void main(int*p) {
  *p = 0;
  Frama_C_dump_each();
}
