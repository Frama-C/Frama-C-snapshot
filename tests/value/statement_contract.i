int A;

/*@ behavior test:
  assumes A == 0;
  ensures \result == 3;
  behavior test2: // invalid
  assumes \false;
  ensures \result == 0;
*/
int main()
{
int d;
d=4;
/*@
  requires d>0; assigns d; ensures d==3;
  behavior foo:
  assumes d == 0;
  ensures d == 42;
*/
d=3;
return d;
}
