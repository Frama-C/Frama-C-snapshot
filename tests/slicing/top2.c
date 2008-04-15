/* run.config
* OPT: -slicing-level 2 -slice-pragma main -slice-print -journal-disable
* OPT: -slicing-level 2 -slice-return main -slice-print -journal-disable
*/


int tab[2]={0, 7 };
int G, X ;
typedef struct {int a; int b; } Ts;
Ts S;

int f(void) {
 volatile int i=0;
 int v;

 v = tab[i];

 G = X;

 return v;
}

int main(void) {
 int x = f();
 G += 1 ;
 //@ slice pragma expr G ;
 return x;
}
