/* run.config_qualif
   OPT: -wp -wp-par 1
*/

struct S { char c; int a[4]; long u; char d; };

struct S s ;
struct S* p ;

/*@
  ensures \forall integer k; 0 <= k < 4 ==>
    \offset( &s.a[k] ) == \offset( &s.a ) + k * sizeof(int) ;
  ensures \forall integer k; 0 <= k < 4 ==>
   \offset( &p->a[k] ) == \offset( &p->a ) ==> k == 0 ;
  ensures \forall integer i,j; 0 <= i <= j < 4 ==>
   \offset( &p->a[i] ) <= \offset( &p->a[j] ) ;
 */
void f(void){return;}
