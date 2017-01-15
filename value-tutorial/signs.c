void main(int v) {
  int x = 1;
  int y = 2;
  int z = -x - y;
  int d = x - y;
  Frama_C_dump_each();

  int q = 1/x;
  Frama_C_dump_each();
  
  if (v != 0) {
    Frama_C_dump_each();
    int r = 1/v;
  }
 
  if (v >= 3 || v <= -4) {
    Frama_C_dump_each();
    int q = 1/v;
  }
}
