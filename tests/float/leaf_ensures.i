double d1, d2, d3, d4;

/*@
  assigns d1, d2, d3;
  ensures \is_finite(\result); 
  ensures \is_finite(d1);
  ensures \is_finite(d2) && -1 < d2 < 1; 
  ensures -1 < d3 < 1;
*/
double d(void);

float f1, f2, f3, f4;
/*@ 
  assigns f1, f2, f3;
  ensures \is_finite(\result); 
  ensures \is_finite(f1); 
  ensures \is_finite(f2) && -1 < f2 < 1; 
  ensures -1 < f3 < 1;
*/
float f(void);

double y;
int main(){
  d4 = d();
  f4 = f();
}
