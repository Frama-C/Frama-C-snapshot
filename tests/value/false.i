/* run.config*
   STDOPT: +"-eva-verbose 2"
*/

/*@ requires i == 1;
  requires i == 1;
  requires i == 1; */
void f (int i);

/*@ ensures \result == 1;
  ensures \result == 1; */
int g (int i) {
  return i;
}

void main (int bla, int bli) {
  int i=0;
  if (bla) f(i);
  if (bli) g(i);
}
