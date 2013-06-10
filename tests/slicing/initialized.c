/* run.config
   OPT: -slice-assert main -then-on 'Slicing export' -print
 **/

void main() {
  int x = 1;
  int y;

 L:
  x = 3;
  y = 2;
  //@ assert \initialized(&x);
  // assert !\initialized{L}(&y);  
}
