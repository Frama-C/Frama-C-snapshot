/* run.config
   OPT: -val -context-width 3 -then -slevel 3
*/


int x, y;
short z;

//@ requires formal != 0;
void main(int c, int *formal) {
  int* px = &x + 1;
  int* px2 = &x + 3;
  int* py = &y + 2;
  short* pz = &z + 2;
  int *p = c ? &x + 1 : &y;
  int *q = p + 1;
  //@ assert \offset(px) == sizeof(int);
  //@ assert \offset(px) != \offset(py);
  //@ assert \offset(px) == \offset(pz);
  //@ assert \offset(q) == \offset(p)+sizeof(int);
  //@ assert \base_addr(px) != \base_addr(py);
  //@ assert \base_addr(px) == \base_addr(px2);
  //@ assert \base_addr(p) == \base_addr(q);
  //@ assert \block_length(p) == \block_length(&x);
  //@ assert \block_length(p) == sizeof(x);
  //@ assert \block_length(&x) > \block_length(&z);
  //@ assert \block_length(formal) >= 4;
  //@ assert \block_length(formal) <= 12;
}
