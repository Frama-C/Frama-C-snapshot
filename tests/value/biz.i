/* run.config*
  GCC:
  STDOPT: #"-main f2"
*/

int *p, *q, G = 0;

void f2() {
  p = &G;
  *(((char*)p)++) = 3; // specific test for biz.c:5: error: invalid lvalue in increment
}
