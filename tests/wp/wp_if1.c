/* run.config
   DONTRUN: fix typing of terms to allow comparisons
*/

int main() {
  int x,c;
  x = x;
  if (!(c==0)) x = 1; else x = 2;
  //@ assert if c then x == 1 else x == 2;
  //@ assert if c>0 then x == 1 else x == 2;
  //@ assert ( (!(c==0)) ==> x == 1) && ((c==0) ==> x == 2);
  return 0;
}
