
typedef struct S {
  int f ;
  int t[10] ;
} ;

struct S s;
int a[10] ;

//@ensures (*s) == { \old(*s) \with .t = a } ;
void f(struct S * s)
{
  /*@loop assigns i,s->t[0..(i-1)] ;
    @loop invariant 0 <= i <= 10 ;
    @loop invariant \forall int k ; 0<=k<i ==> s->t[k] == a[k] ;
    @*/
  for (int i=0; i<10; i++)
    s->t[i] = a[i] ;
}
