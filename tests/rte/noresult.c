/* run.config
   OPT: -rte -rte-print -rte-precond -journal-disable
*/

int x ;
//@ ensures \result > 0 ; assigns x;
int f(void);

//@ ensures \result > 0 ; assigns \nothing;
int g();

//@ requires p > 0 ; ensures \result > 0 ; assigns \nothing;
int h(int p);

void job(void)
{
  f();
  g();
  h(2);
}

