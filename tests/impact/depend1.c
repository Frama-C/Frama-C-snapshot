/* run.config
   STDOPT: +"-impact-pragma main"
   */


int find(int x) { return x; }

int main()
{
  int a = find(1);
  /*@ impact pragma stmt; */
  int b = find(2);
  int c = find(b);
  int d = find(3);
  return c ;
}
