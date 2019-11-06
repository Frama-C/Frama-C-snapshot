/* run.config
   COMMENT: \result
*/

/*@ ensures \result == (int)(x - x); */
int f(int x) { 
  x = 0; 
  return x; }

int Y = 1;

/*@ ensures \result == x;
  @ ensures \result == Y; */
int g(int x) { 
  return x; 
}

/*@ ensures \result == 0; */
int h() { return 0; }

int main(void) {
  f(1);
  g(Y);
  h();
  return 0;
}
