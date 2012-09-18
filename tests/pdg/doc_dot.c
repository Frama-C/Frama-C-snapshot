/* run.config
 OPT: -lib-entry -main g -fct-pdg g -pdg-dot tests/pdg/doc -journal-disable -pdg-print -pdg-verbose 2
*/
/* To build the svg file:
 * dot -Tsvg tests/pdg/doc.g.dot > tests/pdg/doc.g.svg
 */
int G1, G2, T[10];

int f (int a, int b, int c) {
  return a+c;
}

int g (void) {
 int x = f(G1, G2, 0);
 if (0 < x && x < 10)
   T[x] = 0;
 return x;
}
