
int A, B, C;
int u, v, w;

/*@ requires u == argf && v == 0;
    assigns u, v, w \from u;
    ensures u != \result;
*/

int main(int argf, int en1, int en2, int en3, int en4, unsigned int uc, int m, int n) 
{
  int x,y,z,t;

  x = 1;
  /*@ assert x == 1+u; */
  Frama_C_show_each_diff(x - u);
  /*@ requires y != 2;
    @ ensures y == 2;
  */
  y = 2;
  /*@ assert y == 2; */
  z = 3;

  A = en1 ? 0 : 1;
  B = en2 ? 0 : 2;
  if (en3)
    {
      //@ assert A == 0 <==> A != 0 ;
      Frama_C_show_each_then_A_B(A,B);
    }
  else if (en4)
    {
      //@ assert ! (A == 0 <==> B == A) ;
      Frama_C_show_each_elseif_A_B(A,B);
    }
  else 
    {
      //@ assert A == 0 <==> B == A ;
      Frama_C_show_each_else_A_B(A,B);
    }

//@ assert 0 <= m <= n <= 9;
   Frama_C_show_each_mn(m, n);

  int a = 0, b = 1;
  /*@ assert (a || b) == b; */
  /*@ assert (a && a) == a; */
 
  int tt[3];
  tt[0] = 1;
  //@ assert (uc > 0 || tt[uc] == 1) == \true;

  /*@ assert y == z; */
  return z;
}
