volatile c;

unsigned int j, k;

void f() {
  j++;
}

void g() {
  k++;
}

// Strategy on 'if' at the end of the loop. Do not do one branch of the if
// entirely before doing the other one.
void main() {
  int i = 0;
  while(i < 65000) {
    Frama_C_show_each_1(i, j);
    i++;
    if (c) {
      Frama_C_show_each_then(i, j);
      f();
    } else {
      Frama_C_show_each_else(i, j);
      g();
    }
  }
  Frama_C_show_each_end();
}
