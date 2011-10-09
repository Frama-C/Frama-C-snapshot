struct Tstr { int a; int b; }; 
int f (struct Tstr * ps) {
  return ps->a;
}
int main (int x, int y) {
  struct Tstr s = {x, y};
  return f(&s);
}
/* 
Function main:
  \result FROM s.a; 
*/
