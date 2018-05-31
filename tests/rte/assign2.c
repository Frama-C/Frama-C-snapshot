/* run.config
   OPT: -rte -warn-signed-overflow -rte-no-mem -print -rte-precond
*/

int i;
int t[10];

//@ ensures 0 <= \result <= 0;
int any(void);

/*@ assigns i, t[\at(i,Post)];
  @ ensures t[i] == \old(t[\at(i,Here)]) + 1;
  @ ensures \let j = i ; t[j] == \old(t[j]) + 1;
  @*/
void f() {
  i = any();
  t[i]++;
}

int main() {
  f(); f();
  return 0;
}
