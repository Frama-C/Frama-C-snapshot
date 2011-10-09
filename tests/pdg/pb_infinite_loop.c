/* run.config
   GCC:
   OPT: -main test_infinite_loop -fct-pdg test_infinite_loop -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -main test_infinite_loop_2 -fct-pdg test_infinite_loop_2 -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -main test_exit -fct-pdg test_exit -journal-disable  -pdg-print -pdg-verbose 2
*/

/* This test is a problem at the moment because the postdominators are Top
   for the points from which there is no path to the exit.
   It means that we cannot compute the control dependencies in the
   infinite loops...
*/


extern int G;

int test_infinite_loop (void) {
  if (G < 0) {
    int i = 0;
    while (1) {
      if (i % 2)
        G++;
      i++;
      }
    G = G/2; /* dead code */
    }
  return G;
}

int test_infinite_loop_2 (void) {
  int i = 0;
  while (1) {
    if (i % 2)
      G++;
    i++;
  }
  return G; /* dead code */
}

/* At the moment, there is no special things done for exit,
 * As it is seen like a normal call to an external function : no problem...
 */
void exit (int x);
int test_exit (int c) {
  if (c)
    return 1;
  else {
    exit (1);
    return 0;
  }
}
