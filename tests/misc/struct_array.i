struct st1 {
  int a;
  int b;
  int *pp;
  int *p;
};
int *outp;
int x,y,z1,z2,z3,z4;
struct st1 T[22] = { {1,2,0,&x}, {&z1,&z2,&z3,&y},{&z4,2,0,&x},{1,2,0,&x} };
int main (char c) {
  outp = T[c].p;
  *outp = 5;
  z1++;
}
