/* run.config
   STDOPT: +"-impact-pragma main"
   */

#define N 32
extern int ring[N] ;

int find(int x)
{
  for (int k=0; k<N; k++) if (ring[k]=x) return k;
  for (int k=0; k<N; k++) if (ring[k]==0) {
      ring[k] = x ;
      return k;
    }
  return -1;
}

int apply(int x,int y) { return find(x); }

int main()
{
  int a = apply( 1 , 100 );
  /*@ impact pragma stmt; */
  int b = apply( 2 , 200 );
  return a+b ;
}
