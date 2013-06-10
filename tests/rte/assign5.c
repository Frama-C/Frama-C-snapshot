/* run.config
   OPT: -rte -warn-signed-overflow -print -rte-no-all -rte-precond -journal-disable
*/

/* the assigns of f shouldn't parse: 
   in fact both assigns are taken into account 
*/
/*@
  assigns *p \from x;
  assigns *p \from \nothing;
 */
int f(int *p, int x);

/* the assigns of g shouldn't parse: 
   here only assigns \from \nothing is kept
*/
/*@
  assigns *p \from \nothing;
  assigns *p \from x;
 */
int g(int *p, int x);

int main() {
  int i,a;
  int t[10];

  i = 0; 
  a = 0;
  t[0] = f(&i,a); // rte warning: from \nothing + other froms
  t[1] = g(&i,a); // no rte warning since only assigns from \nothing is kept
}
