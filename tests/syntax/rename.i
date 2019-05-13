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

void f5() {
  { int y_0; }
  int y_0;
}

int y_0;

void f6() {
  { int y_2; }
  int y_2;
}

int y_1;

void f7() {
  { int __constr_expr_1 = 0; }
  int __constr_expr_1 = 0;
}

int __constr_expr_0 = 0;

struct not_anon {
    int __anonCompField1;
};

struct not_anon s = {.__anonCompField1 = 0};

struct anon {
  struct { int inner_i; };
};

struct anon a = { .inner_i = 0 };
