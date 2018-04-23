int f() {
  int x = 0;
  { int x_0 = 1;
    { int x = 2;
      return x; }
  }
}

int g() {
  int x_0 = 0;
  { int x_0 = 1;
    return x_0; }
}

void f1() {
  static int i_0 = 0;
  static int i_67 = 1;
  static int i_68 = 2;
  i_67 += i_68+i_0;
}



void f2() {
  static int i_0 = 0;
  static int i_67 = 3;
  static int i_68 = 4;
  i_67 += i_68+i_0;
}

void f3() {
  static int j_0 = 2;
 j_0 ++;
}

void f4(int *j_0_1) {
  int j_0 = *j_0_1;
  { int j_0_1 = j_0;
    j_0_1+=j_0; }
}
