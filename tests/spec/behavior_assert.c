/* run.config 
OPT: -val -deps -out -input -journal-disable -lib-entry
OPT: -val -deps -out -input -journal-disable
*/

int e;

/*@
  behavior b:
  assumes e==0;
*/
void f(void) {
  int x = 1;
  //@ for b: assert \false;
  x = 2;
  //@ for b: assert 1==1;
  x = 3;
}

int G;
/*@
  behavior be:
  assumes e==0;
  ensures G==3;
*/
void g(void) {
  int i=0 ;
  while (i < 3)
    //@ for be: invariant 0<=i<3;
    //@ for be: assert 0<=i<3;
    i++; 
  G = i;
}


int abs(short x) {
  if (x <= 0)
    return -x;
  else return x;
}

/*@
  behavior not_null:
    assumes a != 0;
    ensures \result > 0;
  behavior null:
    assumes a == 0;
    ensures \result == 0;
  complete behaviors not_null, null;
*/
int h1(short a) {
  int r = abs((a-a)+a);
  int r2 = r;
  /*@ for not_null:
    assert r != 0; */
  return r;
}

extern int c;

void h2 () {
  int a, b;
  if (c)
    if (c+1)
      if (c+2)
        a = -2;
      else
        a = 3;
    else
      a = -4;
  else
    a = -1;
  b = h1 (a);
  //@ assert b > 0;
}

/*@
  behavior b:
    assumes e==0;
  behavior c:
    assumes e != 0;
  complete behaviors;
*/
void k(void) {
  //@ for c: assert \true;
  //@ for b: assert \false;
}

void main(int v) {
  if (v) f();
  g();
  h2();
  k();
}
