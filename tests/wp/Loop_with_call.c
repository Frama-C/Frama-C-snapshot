/*@
   assigns \nothing;
   behavior DUMMY : 
   assumes n == 0 ;
   ensures \result == n;
   behavior FOO : 
   ensures \result == n-4+4; 
*/
int find_behav(int n)
{
  /*@
    loop assigns i;
    loop invariant 0 <= i <= n;
    loop   variant n-i;
  */
  for (int i = 0; i < n; i++) ;
  return n;
}

//@ ensures \result == 0; 

int call_find_behav(void)
{
  int i = find_behav(0); 
  return i;
 
}
int main (void) { return 0 ; }
