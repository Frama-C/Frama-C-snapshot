/* run.config
   OPT: -check -slice-return main -slice-undef-functions -journal-disable -then-on 'Slicing export' -print
*/

int x,y,z;

int X, Y ;

/*@ requires a > 0;
    ensures y == a; */
int f(int a) { y = x; return x; }

/*@ 
    requires a > 0;
    requires b > 0;
    assigns \result \from a;
    assigns Y \from b;
*/
int g (int a, int b);
 
/*@ requires x > 0;
    ensures X > \old(X);
    ensures Y == \old(Y) + 1; 
 */
void k(int x) {
  X += x ;
  Y ++ ;
}

int main() {
  x = 1;
  y = 2;
  z = f(x);
  z += g(1, 2);
  k(3);
  return X + z;
}
