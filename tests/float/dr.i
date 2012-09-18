/* run.config
  STDOPT: +"-val"
  STDOPT: +"-val -float-hex"
*/

float big  = 100e30f;
float big2 = 100.126E30f;

float ne1 = -0.1f, ne2 = -.5f, nodigits = 10.f;
float smaller = 1e-99999999999999999999999f; 
//  larger  = 1e99999999999999999999999f; causes initial state to bottomify
float he = 0X1.8p1f;
float g1 = 0.1 ;
float f1 = 0.1f, f9 = 0.999999999999999999f, ep = 1.25e+10f;

float  g2 = 1.01161128282547 ;
float  f2 = 1.01161128282547f;
double d2 = 1.01161128282547 ;

int e1, e2;

int printf(const char *format, ...);

main(){
  e1 = f1 == g1;
  e2 = f2 == g2;
  printf("%d %d\n", e1, e2);
  return 0;
}
