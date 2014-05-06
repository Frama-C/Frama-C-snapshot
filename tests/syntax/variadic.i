
int normal(int n);
int vf(int x, ...);
typedef char tt;
struct T {int a;} st;
tt abstract;
unsigned char uchar;
signed char chr;
unsigned short ushort;
unsigned long long ll;
long double ld;
double d;

void h();

void g() {
  vf(1,1u,uchar,3.0f, ushort, ll, abstract, st, ld,d); 
  // vf() is variadic: the default argument promotions apply after the initial
  // arguments. C99 6.5.2.2:7
  f(1,uchar); // f undeclared, default argument promotions apply C99 6.5.2.2:6
  h(1,uchar); // h declared but without a prototype: C99 6.5.2.2:6
  normal(uchar);
}
