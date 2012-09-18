typedef int t[10];
typedef int u[4];

void main () {
  int tab1[4];
  u* p = &tab1;
  t* p2 = (t) p;
}
