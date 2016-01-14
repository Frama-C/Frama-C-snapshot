/* run.config
   GCC:
   STDOPT: #"-main f -absolute-valid-range 0x1-0xFFFFF"
   STDOPT: #"-main f1 -absolute-valid-range 0x1-0xFFFFF"
*/
char **c,a,*b,**y;
int x;

int f() {
  a = 'b';
  b = &a;
  c = &b;
  x = (int)c;
  y = (char**)x;
  *((char**)0x12) = &b;
  **((char**)0x12)='a';
  return 0;
}

int f1() {
  *((char*)17) = 27;
  *((char*)19) = 29;

  x = c?17:19;
  b = (char*)x;
  *b = 0;
  return 0;
}
