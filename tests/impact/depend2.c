/* run.config
   STDOPT: +"-impact-pragma main"
   */

int find(int x) { return x; }

int apply(int x,int y) { return find(x)+y; }

int main()
{
  int a = apply(1,100);
  /*@ impact pragma stmt; */
  int b = apply(2,200);
  return a+b ;
}
