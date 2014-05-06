/* run.config
   STDOPT: +"-scope-verbose 2 -remove-redundant-alarms -context-width 3"
   */


int u,v,w;

int main(int x,int *p) {
  /*@ assert x >=0; */
  /*@ assert \valid(p+1); */
  /*@ assert \valid_read(p+2); */
  *(p+1)=x;

  return x+*(p+2);
}
