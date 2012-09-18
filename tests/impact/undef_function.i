/* run.config
   STDOPT: +"-impact-pragma main"
   */
int y;
void g(int);

int main() {
  /*@ impact pragma stmt; */
  y=2;
  g(y);
  return y;
}
