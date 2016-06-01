void main1(float f) {
  Frama_C_show_each_1(f);
  if (-f >= 3.) {
    Frama_C_show_each_1_then(f);
  } else {
    Frama_C_show_each_1_else(f);
  }
}

void main2(int i) {
  Frama_C_show_each_2(i);
  if (-i >= 3) {
    Frama_C_show_each_2_then(i);
  } else {
    Frama_C_show_each_2_else(i);
  }
}

volatile v;

void main() {
  main1((float)v);
  main2(v);
}
