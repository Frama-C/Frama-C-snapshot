/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-par 1
*/

/*@ requires HP: ok: 0 <= i && i <= j && j-i == 10; 
    assigns p[i..j] ; 
    ensures P: ok: \valid (p+(i..j)) ;
 */
void val_assigns_p(int * p , int i, int j);

/*@ requires HQ: ok: 0 <= k && k <= l && l-k == 10;
    assigns q[k..l]; 
    ensures Q: ok: \valid(q+(k..l)) ;
 */
void val_assigns_q(int * q,int k, int l); 

int * r;

/*@
 requires H1 : a <=b && 0 <= a && b-a == 10;
 requires H2 : c <=d && 0 <= c && d-c == 10; 
 ensures  P1 : ok: 
          0 <= a && b < 10 && 10 <= c && d < 20 ==>
         \valid(r+(a..d)) && \separated (r+(a..b) , r+(c..d)) ; 
 ensures  P2 : ok: 
          0 <= c && d < 10 && 10 <= a && b < 20 ==>
         \valid(r+(c..b)) && \separated (r+(a..b) , r+(c..d)) ;
*/

void test (int a, int b , int c, int d)
{
  val_assigns_p(r,a,b) ; 
  val_assigns_q(r,c,d) ;
}
