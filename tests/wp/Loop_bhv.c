/*@
   assigns \nothing;
   ensures \result == n;
*/
int find(int n)
{
  /*@
    loop assigns \nothing;
    loop invariant 0 <= i <= n;
    loop   variant n-i;
   */
  for (int i = 0; i < n; i++) ;
  return n;
}


/*@
   assigns \nothing;
   behavior DUMMY : 
   ensures \result == n;
   behavior FOO : 
   assigns \nothing;
   ensures \result == n; 
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

int G; 
//@ ensures \at(G,Old) == \at(G,Here);
void skip(void) 
{
  int i ; i++;
}


//@ ensures \result == 0; 
int call_find_behav()
{
  int i = find_behav(0); 
  return i;
 
}


/*@ 
   assigns G; 
   ensures G == 5;
*/ 
int res(int n)
{
  G =5;
  /*@
    loop assigns \nothing;
    loop invariant 0 <= i <= n;
    loop   variant n-i;
   */
  for (int i = 0; i < n; i++) ;
  return n;
}

/*@ 
   assigns G; 
   behavior TEST:
   ensures G == 5;
*/ 
int res_with_behavior(int n)
{
  G =5;
  /*@
    loop assigns \nothing;
    loop invariant 0 <= i <= n;
    loop   variant n-i;
   */
  for (int i = 0; i < n; i++) ;
  return n;
}


int main (void) { return 0 ; }
