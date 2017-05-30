/* run.config*
   STDOPT: +"-print"
*/



_Bool x;
int y;

int main() {
  x=0;
  Frama_C_show_each(x);
  x=2;
  Frama_C_show_each(x);
  y=x+1;
  Frama_C_show_each(x, y);
  x=x+1;
  Frama_C_show_each(x);
  x=x+1;
  Frama_C_show_each(x);
  return y;
}
