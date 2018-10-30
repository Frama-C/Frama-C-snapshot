/* run.config
   OPT: -rte -warn-signed-overflow -rte-no-mem -print -journal-disable
*/

// if f() assigns i there might be a problem 
//@ assigns \nothing;
int f(void);

int main() {
  int i;
  int t[10];

  i = 0; 
  t[i] = f();
}
