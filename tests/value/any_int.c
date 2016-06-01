
// This very strange code was used to generate any possible integer a long
// time ago. It is kept only to test relational domains that may erroneously
// learn information about volatile l-values.
int main() {
  volatile int y=0;
  int x=0;

  while(y) {y++;y++; if (y-1) x++; else x--;}
  return x;
}
