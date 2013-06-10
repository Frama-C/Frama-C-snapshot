/* run.config
   STDOPT: +"tests/spec/merge_different_assigns_bis.i"
*/

int x, z, t, u, v, w;

/*@ assigns x;
  assigns z \from x;
  assigns u;
  assigns t \from \nothing;
  assigns w;
  assigns x \from x;
*/
int f(int y);
