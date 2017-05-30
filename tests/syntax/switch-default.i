int main() {
  int x = 42;
  for (int i = 0; i < 4; i++) {
    switch (x)
    default: {
      Frama_C_show_each_i(i);
      break;
    }
  }
  return 0;
}

void f () {
  int i;
  switch (0) {
    case 0: { i=1; break; }
    default: i=9;
  }
  return;
}
