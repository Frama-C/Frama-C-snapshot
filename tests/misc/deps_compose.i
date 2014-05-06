int c, z, x1, y1, x2, y2, x3, y3, z, x4, y4, x5, y5, x6, y6;
int c = 1;

void f() {
  if (c) {
    x1 = y1;
    x2 = y2;
    x3 = y3;
    x4 = y4;
    x5 = y5;
    x6 = y6;
  }
}

void main() {
  if (c) {
    f();
  }
  y1 = z;
  y2 = z;
  y3 = z;
  y4 = z;
  y5 = z;
  y6 = z;

  f(); // Previous versions of Frama-C recognized that the 'from' for x1..x6
       // before and after the call were identical, and merged them. This is
       // incorrect, as those of the call must be substituted (here, changed
       // into 'From z')
}
