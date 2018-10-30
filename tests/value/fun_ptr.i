/* run.config*
  STDOPT:
  STDOPT: +"-machdep msvc_x86_64"
*/


int f(int x)
{
  return x+1;
}

int g(int x, int y)
{
  return x+y;
}

typedef int (*fptr1)(int);
typedef int (*fptr2)(int, int);
typedef double (*fptr3)(int);

long long t[2] = { (long long)&f, (long long)&g };

int R1, R2;
double R3;

void test1(int nd)
{
  R1 = ((fptr1)(t[nd]))(3);
}

void test2(int nd)
{
  R2 = ((fptr2)(t[nd]))(3, 4);
}

void test3(int nd)
{
  R3 = ((fptr3)(t[nd]))(5);
}

double h(short a, short b) {
  return a + b;
}

volatile int v;

void benign(int j, void *p) {
  int *q = p;
  *q = j; // q is a void*, which is actually an int, but at the call site it is a short *. We accept this for now.
  int k = j+0; 
}

void test_benign () {
  int x;
  void (*p) (long, short *) = &benign; // We accept this cast, because the arguments are "compatible enough". An (unprovable) alarm is still emitted
  (*p)(1U << 31U, &x);
}

void too_much(int i) {
  int j = i;
}

void too_much2(int i, int j, int k) {
  int l = i+j+k;
}

void test_too_much_benign () {
  int x;
  void (*p) () = &too_much;
  (*p)(1, 2); // Accepted (with an alarm)
  if (v) {
    p = &too_much2;
    (*p)(1, 2); // Failure    
  }
}

int main(){
  test1(!v);
  test2(!v);
  if (v) test3(!v);
  double (*ph)() = h;
  if (v)
    ph(1., 2.);
  if (v)
    ph();
  if (v)
    ph((short)1, (short)2);

  test_benign();
  test_too_much_benign();

  return 0;
}

