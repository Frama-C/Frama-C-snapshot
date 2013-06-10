/* run.config
   OPT: -rte -warn-signed-overflow -rte-no-all -print -rte-precond
*/

/*@
  requires \valid(x);
  requires \valid_range(x, 0, 10);
  assigns *x \from y;
  assigns \result \from *x;
*/
int g(int *x, int y);

/*@
  requires \valid(&tab[0]);
  assigns \nothing;
*/
int f(int* tab) { return 0; }

int main() {
  
  int tab[2] = { 3, 4 };
  int a = f(tab);
  return g(&tab[3 - tab[0]] + a, a);

}
