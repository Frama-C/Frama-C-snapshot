/* run.config
   STDOPT: +"-impact-pragma main"
   */

int f(int, ...);

int main () {
  int i=0;
  /*@ impact pragma stmt; */
  i++;
  f(i);
}
