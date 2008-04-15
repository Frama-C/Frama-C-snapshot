/*@ requires \valid(a);
 @ behavior az :
 @   assumes *a == 0;
 @   ensures *a == 0; */
void f(int *a) {
 if (*a != 0) *a = 1;
}

/*@ requires \valid(a);
 @ ensures \old(*a) == 0 ==> *a == 0; */
void g(int *a) {
 f(a);
}
