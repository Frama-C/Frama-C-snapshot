

int u,v,w;

/*@ requires u==argf && v==0;
    assigns u,v,w \from u;
    ensures u!=\result;
*/
int main(int argf) {
  int x,y,z,t;

  x = 1;
  /*@ assert x == 1+u; */
  Frama_C_show_each_diff(x - u);
  /*@ requires y!= 2;
    ensures y == 2;
  */
  y = 2;
  /*@ assert y == 2; */
  z = 3;
  /*@ assert y == z; */
  return z;
}
