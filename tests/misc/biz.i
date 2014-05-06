/* run.config
  GCC:
  OPT: -val -deps -out -input  -main f2 -journal-disable
*/

int *p, *q, G = 0;

void f2() {
  p = &G;
  *(((char*)p)++) = 3; // specific test for biz.c:5: error: invalid lvalue in increment
}
