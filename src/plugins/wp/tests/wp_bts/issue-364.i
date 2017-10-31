
void main() {
  char *a = "abc";
  // Validity of ending `\0` ;
  //@ assert ZERO: \valid_read(a+3);
  //@ assert OVER: !\valid_read(a+4);
}

