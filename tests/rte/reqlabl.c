/* run.config
   OPT: -rte -rte-print -rte-precond -journal-disable
*/

//@ requires PROP_SUR_982: x>0;
int f(int x);

void g(int a)
{
int c;
c = f(a);
} 
