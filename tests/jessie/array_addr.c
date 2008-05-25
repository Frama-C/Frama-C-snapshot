
int t[3][3];

/*@ ensures *p == 0; */
void h(int *p);

/*@ requires \valid_index(t[1],2); */
void f() {
  h(t[1]);
}

/*@ requires \valid_index(t + 1,2); */
void g() {
  h(t + 1);
}

int t2[3][3];

/*@ logic int deref{Current}(int* p) = \at(*p,Current); */

/*@ ensures deref(&t2[1][2]) == 0; */
void m() {
  h(&t2[1][2]);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make array_addr"
End:
*/
