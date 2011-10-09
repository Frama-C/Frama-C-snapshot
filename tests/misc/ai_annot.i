/* run.config
   STDOPT: +"-remove-redundant-alarms"
   */


int u,v,w;

int main(int x,int *p) {
  /*@ assert x >=0; */
  /*@ assert \valid(p); */
  /*@ assert \valid(p+1); */
  *p=x;

  return x+*(p+1);
}
