struct Tstr { int a; int b; };

int f (struct Tstr * ps) {
  return ps->a;
}

int f3(int*p) { return *p ;} 

int main (int x, int y) {
  struct Tstr s = {x, y};
//  return f3(&s);
  return f(&s);
}

int f2 (struct Tstr s) {
  return s.a;
}

int main2 (int x, int y) {
  struct Tstr s = {x, y};
  return f2(s);
} 
