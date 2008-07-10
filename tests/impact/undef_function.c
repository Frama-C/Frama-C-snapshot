/* run.config
   GCC:
   OPT: -impact-pragma main -impact-print
   */
int y;
 
int main() {
  /*@ impact pragma stmt; */
  y=2;
  g(y);
  return y;
}
