/* run.config
   GCC:
   OPT: -lib-entry -main simple -fct-pdg simple -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main simple_with_break -fct-pdg simple_with_break -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main infinite -fct-pdg infinite -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main infinite2 -fct-pdg infinite2 -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main maybe_infinite -fct-pdg maybe_infinite -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main two_infinite_loops -fct-pdg two_infinite_loops -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main loop_with_goto -fct-pdg loop_with_goto -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main non_natural_loop -fct-pdg non_natural_loop -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -lib-entry -main dead_code -fct-pdg dead_code -journal-disable  -pdg-print -pdg-verbose 2



   */
/*
Choix de la fonction sur laquelle on travaille :

F=maybe_infinite

Pour voir le CFG :

bin/toplevel.opt -lib-entry -main $F -deps -verbose tests/pdg/loops.c
zgrviewer ./$F_cfg.dot

Pour voir les postdominateurs :
bin/toplevel.opt -lib-entry -main $F -fct-pdg $F -dot-postdom p tests/pdg/loops.c ;
zgrviewer ./p.$F.dot

Pour voir le PDG :
bin/toplevel.opt -lib-entry -main $F -fct-pdg $F -pdg-dot pdg tests/pdg/loops.c ;
zgrviewer ./pdg.$F.dot

*/

int after;

int simple (int n) {
  int s = 0;
  int i = 0;
  while (i < n) {
    s += 2;
    i++;
    }
  after = 0;
  return s;
}
int simple_with_break (int n) {
  int s = 0;
  int i = 0;
  while (1) {
    if (i < n) {
      s += 2;
      i++;
      }
    else
      break;
  }
  after = 0;
  return s;
}
int infinite (int n) {
  int s = 0;
  int i = 0;
  while (1) {
    s += 2;
    i++;
    }
  after = 0;
  return s;
}
int infinite2 (int n) {
  int s = 0;
  int i = 1;
  while (i) {
    s += 2;
    }
  after = 0;
  return s;
}
int maybe_infinite (int n) {
  int s = 0, i = 0;
  if (n > 0) {
    while (1) {
      i+=1;
      if (s < 10)
	s += 2;
      i+=2;
      }
    }
  else
    s = 1;
  after = 0;
  return s;
}
int two_infinite_loops (int n) {
  int s = 0, i1 = 0, i2 = 0;
  if (n > 0) {
    while (1) {
      i1 += 1;
      if (s < 10)
	s += 2;
      else {
	i2 = 0;
	while (1) {
	  i2++;
	  }
        i2+=2;
        }
      i1+=2;
      }
    }
  else
    s = 1;
  after = 0;
  return s;
}
int loop_with_goto (int n) {
    if (n > 0) {
L :   n--;
      if (1) goto L;
    }
    return n;
}
/* this function is similar to [test_ctrl_dpd_multiple] in
 * [tests/pdg/dpds_intra.c] but the value analysis converges,
 * so we can see that [x=x+2;] has a control dependency on both [n<0] and [x<n].
 */
int non_natural_loop (int n) {
  int x = 1;
  if (n < 0) {
    x = 0;
    n = 10;
    }
  else {
    n = 20;
    L : x = x + 2;
    }
  if (x < n)
    goto L;
  return x;
}
int dead_code (int n) {
  int x = 0;
  goto L;
W : x++;
    if (n > 0) goto W;
L: x+=n;
   return x;
}
