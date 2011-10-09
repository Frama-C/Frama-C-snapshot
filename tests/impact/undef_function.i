/* run.config
   GCC:
   STDOPT: +"-impact-pragma main"
   */
int y;

int main() {
  /*@ impact pragma stmt; */
  y=2;
  g(y);
  return y;
}
