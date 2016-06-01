/* run.config*
   GCC:
   OPT: -val @VALUECONFIG@ -deps -out -input -journal-disable
*/

int t[4];

int *p;
int *q;

void f(void)
{
  *p = 4;
  *q = 5;
}

int A,B,C;
void main(int a, int b, int *pp)
{

  Frama_C_show_each_f(pp);

  //@ assert \valid(pp);

  Frama_C_show_each_f(pp);

  *pp = 5;

  //@ assert *pp + 1 == 6;

  A = 10;
  B = 11;

  p = &A;
  q = &B;
  f();

  p = &A;
  q = &A;
  f();

}
