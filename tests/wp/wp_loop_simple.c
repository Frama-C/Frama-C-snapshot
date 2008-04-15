
/*@ ensures \result == 5;
*/
int main (int c) {
  int x;
  int c = 0;

  //@ loop invariant 0 <= c <= 6 && ((c==0 || x == c-1));
  for(c=0;c<=5;) {
    //   CEA_DUMP();
    x = c;
    c++;
    //   CEA_DUMP();
  }
// CEA_DUMP();
//@ assert c == 6;
  return x;
}
