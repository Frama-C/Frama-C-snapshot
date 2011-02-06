typedef enum _Color { Blue, Red, Black = 0 } Color;

int int_of_color (Color col) {
  int x;
  if (col != Black) {
    x = col; //@ assert x != 0;
  }
  else {
    x = Black; //@ assert x == Blue; // strange property, isn't it?
  }
  return x;
}


//@ ensures \exists Color c; \result == c; // strange property, isn't it?
Color unspecified_color (void) {
  return 10; // Unfortunatly, this is not forbidden!
}

//@ ensures \result == Blue;
Color enum_in_annot (void) {
  return Blue;
}
