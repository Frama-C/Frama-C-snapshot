extern unsigned int i1, i2;

int main(int c) {
  //@ assert i1 > 10;
  //@ assert i2 > 10;
  unsigned int v = i1 + i2;

  if (c) {
    unsigned int w = -i1;
    Frama_C_show_each_dead ();
  }
}
