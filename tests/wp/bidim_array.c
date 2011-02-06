/* 
   kind : Positive
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Out of Scope
 */

int a[5][10] = {0, };

/*@ assigns a[..][..]; 
    ensures a[1][3] == 42;
 */
void f(void)
{
  a[1][3] = 42;
}

int x[3][3] = { {1, 2, 3} , {4, 5, 6} , {7, 8, 9} };

//@ requires x[0][1] == 2; ensures \result == 2;
int f0 (void) {
  int * p = &(x[0][0]);
  p++;
  return *p;
}

//@  requires x[1][0] == 4; ensures \result == 4;
int f1 (void) {
  int (*pt)[3] = x;
  pt++;
  return (*pt)[0];
}
//@ requires x[1][1] == 5; ensures \result == 5;
int f2 (void) {
  int (*pt)[3] = x + 1;
  int * p = (*pt) + 1;
  return *p;
}

//@ requires x[0][1] == 2; ensures \result == 2;
int read_tab (void) {
  return x[0][1];
}

int main (void){return 0;}
