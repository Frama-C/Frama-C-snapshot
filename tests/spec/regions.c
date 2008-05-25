
/*@ requires \valid(p);
  @ ensures *p == n;
  @*/
void g(int *p, int n) {
  *p = n;
}

/*@ requires \valid(x) && \valid(y);
  @ ensures *x == 1 && *y==2;
  @*/
void f(int *x, int *y) {
  g(x,1);
  g(y,2);
}

struct S {
  int t1[2];
  int t2[2];
};

//@ ensures s.t1[0]==1 && s.t2[0]==2 && s.t1[1]==2 && s.t2[1]==1;
void main(struct S s) {
  f(&s.t1[0],&s.t2[1]);
  f(&s.t2[0],&s.t1[1]);
  f(&s.t1[0],&s.t1[0]);
}

/* on veut :

zones globales :

 Zone 0: {s.t1[0]; };
 Zone 1: {s.t1[1]; };
 Zone 2: {s.t2[0]; };
 Zone 3: {s.t2[1]; };

zones locales :

 f:
 Zone 4: { *x; }
 Zone 5: { *y; }

 g:
 Zone 6; { *p; }

Appels:

 g(..) ligne 13: zone 6 -> zone 4
 g(..) ligne 14: zone 6 -> zone 5
 f(..) ligne 24: zone 4 -> zone 0, zone 5 -> zone 2
 f(..) ligne 25: zone 4 -> zone 3, zone 5 -> zone 1
 f(..) ligne 26: zone 4 -> zone 0, zone 5 -> zone 0

*/
