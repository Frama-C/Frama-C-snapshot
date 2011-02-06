
typedef struct S {
  int r ;
  int f[7] ;
  int g[5] ;
} ;


int a[10] ;
struct S s ;

void f(void)
{
  //@assert s.g == s.f + 7 ;
}
