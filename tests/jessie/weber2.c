
/*@
requires last > first;
requires \valid_range(first, 0, last-first-1);
ensures *\result == value;
*/
int* find_int_array (int* first, int* last, int value ) {}

void f() {
  int *i, *j, k, *l;
  l = find_int_array(i,j,k);
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j weber2"
End:
*/
