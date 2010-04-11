int a,b;

/*@
  behavior b:
  ensures a!=0;
  ensures a==0;
  assigns a;
  behavior c:
  ensures a==0;
  ensures a==0;
  assigns a;
  behavior d:
  assumes a==0;
*/
void f(void) {
  a=0;
}
