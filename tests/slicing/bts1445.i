/*  run.config
OPT: -check -slice-calls main -then-on "Slicing export" -print 
OPT: -check -slice-calls f -main f -then-on "Slicing export" -print 
*/
int x = 0;

int main() {
  while(1) 
    x=0;
  return x + 1;
}

int f() {
  while(1) 
    x=0;
  return x + 1;
}
