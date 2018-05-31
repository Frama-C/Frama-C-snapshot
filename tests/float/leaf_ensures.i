double d1, d2, d3, d4, d5;

/*@
  assigns d1, d2, d3, \result \from \nothing;
  ensures \is_finite(\result); 
  ensures \is_finite(d1);
  ensures \is_finite(d2) && -1 < d2 < 1; 
  ensures -1 < d3 < 1;
*/
double d(void);

float f1, f2, f3, f4, f5, f6;
/*@ 
  assigns f1, f2, f3, \result \from \nothing;
  ensures \is_finite(\result); 
  ensures \is_finite(f1); 
  ensures \is_finite(f2) && -1 < f2 < 1; 
  ensures -1 < f3 < 1;
*/
float f(void);

typedef float FLOAT;

/*@ assigns \result \from \nothing;
  ensures \is_finite(\result); */
FLOAT g(void);

/*@ assigns \result \from \nothing;
    ensures \is_finite((float)\result); // Always true. */
int h(void);

double y;
int main(){
  d4 = d();
  f4 = f();
  f5 = h(); // 2^31 - 1 is "rounded" to 2^31. by the assignment
  d5 = h();
  f6 = g();
}
