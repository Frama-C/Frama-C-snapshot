int A,B;

int main(int c, int d)
{
  A = !!d;
  /*@ assert ((A ==> \false) ==> \false); */

  /*@ assert c ==> \false; */
  return 1 + c;

}