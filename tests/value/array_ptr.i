int G = 1;

typedef int param_check[20];

int f(param_check **x) {
  G=(**x)[0];
  (**x)[0] = 2;
  return 2;
}

param_check l={1};

int main() {
  int g = (int) &l;
  f((param_check **)&g);
}
