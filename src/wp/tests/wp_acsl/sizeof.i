struct S { int x; char c; int y; } ; // There is some padding here !

struct S a ;
struct S b[4] ;

/*@ predicate eq(integer a,integer b) = (a==b) ; */

void foo(void)
{ 
  /*@ assert A: !eq( sizeof(a) , 2 * sizeof(int) + sizeof(char) ); */
  /*@ assert B: eq( sizeof(b) , 4 * sizeof(a) ); */
}

