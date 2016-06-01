/* This is a test for the zero-sized arrays of GCC. */
struct foo { int x; int y[0]; };
struct bar { struct foo z; };

void main() {
unsigned char T[100];
  struct foo * F=T;
  F->x=4;
  F->y[0]=5;
}
