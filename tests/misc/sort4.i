/* run.config
  GCC:
  OPT: -val -deps -out -input  -lib-entry -main sort4_1 -journal-disable
  OPT: -val -deps -out -input  -lib-entry -main sort4_4 -journal-disable
  OPT: -val -deps -out -input  -lib-entry -main sort4_3 -journal-disable
*/

/* sort 4 integers */

int a, b, c, d;

void sort4_1() {

  int tmp;
  if (a > b) { tmp = a; a = b; b = tmp; }
  if (c > d) { tmp = c; c = d; d = tmp; }
  if (a > c) { tmp = a; a = c; c = tmp; }
  if (b > d) { tmp = b; b = d; d = tmp; }
  if (b > c) { tmp = b; b = c; c = tmp; }
  /*@ assert a <= b <= c <= d; */
}



/*@ requires \valid_range(t,0,3);
    ensures t[0] <= t[1] <= t[2] <= t[3]; */
void sort4_4(int t[4]) {
  int tmp;
  if (t[0] > t[1]) { tmp = t[0]; t[0] = t[1]; t[1] = tmp; }
  if (t[2] > t[3]) { tmp = t[2]; t[2] = t[3]; t[3] = tmp; }
  if (t[0] > t[2]) { tmp = t[0]; t[0] = t[2]; t[2] = tmp; }
  if (t[1] > t[3]) { tmp = t[1]; t[1] = t[3]; t[3] = tmp; }
  if (t[1] > t[2]) { tmp = t[1]; t[1] = t[2]; t[2] = tmp; }
}


/* commented because of memory explosion */
#if 0
/*@ requires \valid(a) && \valid(b) && \valid(c) && \valid(d) &&
  @   a != b && a != c && a != d && b != c && b != d && c != d;
  @ ensures *a <= *b <= *c <= *d; */
void sort4_2(int *a, int *b, int *c, int *d) {
  int tmp;
  if (*a > *b) { tmp = *a; *a = *b; *b = tmp; }
  if (*c > *d) { tmp = *c; *c = *d; *d = tmp; }
  if (*a > *c) { tmp = *a; *a = *c; *c = tmp; }
  if (*b > *d) { tmp = *b; *b = *d; *d = tmp; }
  if (*b > *c) { tmp = *b; *b = *c; *c = tmp; }
}
#endif



/*@ predicate swap_ord(int a2,int b2,int a1,int b1) =
  @   (a1 <= b1 ==> (a2 == a1 && b2 == b1)) &&
  @   (a1 > b1 ==> (a2 == b1 && b2 == a1)) ;
  @*/

/*@ requires \valid(a) && \valid(b) && \valid(c) && \valid(d) &&
  @   a != b && a != c && a != d && b != c && b != d && c != d;
  @ ensures *a <= *b <= *c <= *d; */
void sort4_3(int *a, int *b, int *c, int *d) {
  int tmp;
  // assigns *a,*b,tmp; ensures swap_ord( *a,*b,\old( *a),\old( *b));
  if (*a > *b) { tmp = *a; *a = *b; *b = tmp; }
  // assigns *c,*d,tmp; ensures swap_ord( *c,*d,\old( *c),\old( *d));
  if (*c > *d) { tmp = *c; *c = *d; *d = tmp; }
  // assigns *a,*c,tmp; ensures swap_ord( *a,*c,\old( *a),\old( *c));
  if (*a > *c) { tmp = *a; *a = *c; *c = tmp; }
  // assigns *b,*d,tmp; ensures swap_ord( *b,*d,\old( *b),\old( *d));
  if (*b > *d) { tmp = *b; *b = *d; *d = tmp; }
  // assigns *b,*c,tmp; ensures swap_ord( *b,*c,\old( *b),\old( *c));
  if (*b > *c) { tmp = *b; *b = *c; *c = tmp; }
}
