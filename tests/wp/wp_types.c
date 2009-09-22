enum Colors { Blue, Red, Black = 0 };

/* TODO */
unsigned char uchar_range (unsigned char i) {
  //@ assert i >= 0;
  //@ assert i <= 255;
  return i;
}

int int_of_color (enum Color col) {
  int x;
  if (col != Black) {
    x = col; //@ assert x != 0;
  }
  else {
    x = Black; //@ assert x == 0;
  }
  return x;
}
