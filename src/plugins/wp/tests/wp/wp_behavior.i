/* run.config
OPT: -wp-no-precond-weakening
OPT: -wp-precond-weakening
*/
/* run.config_qualif
DONTRUN:
*/

int tab[10], x, y;

/*@ axiomatic a {
  @ predicate P reads \nothing;
  @ predicate Q reads \nothing; predicate RQ reads \nothing;
  @ predicate R reads \nothing;  predicate R1 reads \nothing;
  @ predicate CX reads \nothing;
  @ predicate RX reads \nothing; predicate RX1 reads \nothing;
  @ predicate PX reads \nothing;
  @ predicate CY reads \nothing;
  @ predicate RY reads \nothing;
  @ predicate PY reads \nothing;
  }*/

/*@ requires R;
  @ ensures P;
  @ behavior X:
  @   assumes CX;
  @   requires RX;
  @   ensures PX;
  @   assigns *qx;
  @ behavior Y:
  @   assumes CY;
  @   requires RY;
  @   ensures PY;
  @   assigns *qy;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
void behaviors(int c, int* px, int *py, int * qx, int* qy) {
   /*@ assert Q;*/
  if (c) *px=1;
  else *py=1;
  return;
}

/*@ requires R;
  @ requires R1;
  @ behavior X:
  @   assumes CX;
  @   requires RX;
  @   requires RX1;
  @   assigns *p;
  @ behavior Y:
  @   assumes CY;
  @   requires RY;
  @   assigns *q;
  @*/
void main(int c, int* p, int *q) {
  if (c) *p=1;
  else *q=1;
  return;
}


/*@ requires RQ ;
   assigns *px, *py ;
   ensures Q ;
*/
void call(int cond, int* px, int *py, int * qx, int* qy) {
  behaviors (cond, px, py, qx, qy) ;
}
