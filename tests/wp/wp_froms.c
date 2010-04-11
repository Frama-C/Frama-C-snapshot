//@ predicate A (int * p);

struct Tstr {int a; int b; int c; } S;

/*@ assigns \result \from S;
    ensures A (\result);
*/
int * maxS (void) {
  int * p = &(S.a);
  if (*p < S.b) p = &(S.b);
  if (*p < S.c) p = &(S.c);
  return p;
}

int x ,y ,z , * p ;

//@ predicate P (int v);

/*@
     behavior c_true :
       assumes c ;
       assigns x \from y ;
       assigns p \from \nothing ;

       ensures P(x);
       ensures x == y + 1;

     behavior c_false :
       assumes ! c ;
       assigns z \from x ;
       assigns p \from \nothing ;
*/
void f ( int c ) {
  if ( c ) p = & y ; 
  else p = & z ;

  if (! c ) * p = x + 1; 
  else x = * p + 1; 
}

int main (void) { return 0 ; }
