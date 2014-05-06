/* run.config
STDOPT: +"-occurrence"
*/

int x,y;

/*@ predicate foo{L} = x == y; */

/*@ assigns \result \from \nothing;
  ensures \result == x + 1;
*/
int f(int x);

int main () { 
  int y = 0;
  int z = f(y);
  y = f(z);
  return f(y);
}
