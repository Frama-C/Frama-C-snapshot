
void main() {
  int i = 0;
  int j = 10;
  /*@ loop variant 10 - i; */
  while (1) {
    if (i == j) break;
    i++;
  }
}

/* 
Local Variables:
compile-command: "LC_ALL=C make loop_eq"
End:
*/
