/* run.config
   EXECNOW: make -s tests/pdg/sets.opt
   CMD: tests/pdg/sets.opt
   OPT: -lib-entry -main f -pdg-debug -pdg -inout -journal-disable
*/

int b, c, x, y, z, t;
void f(int a) {
  y = 0;        // 1, node 9
  if (a) y = 1; // 2 puis 3, (y = 1: node 11)
  z = y;        // 5
  y++;          // 6 (node 14)
  x = z;        // 8
  b = a;        // 9
  t = b + y;    // 11
  y = 5;        // 12
  c = 8;        // shouldn't have any relation with node 14...
}
