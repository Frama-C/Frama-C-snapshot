# pragma SeparationPolicy(none)
/*@
   requires \valid(p); 
   requires \valid(q);
   ensures *p==\old(*p)+1;
   ensures *q==\old(*q)+1;
 */
void ptr_incr(int *p,int *q)
{
  *p+=1; *q+=1; 
}
int main (void) { return 0 ; }
