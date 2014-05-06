/* run.config
   OPT: -slice-return main -journal-disable -then-on 'Slicing export' -print
COMMENT: TODO add -check to the command, but it fails at the moment...
   OPT: -main bts906b -fct-pdg bts906b -pdg-print -pdg-verbose 2
   OPT: -main bts906c -fct-pdg bts906c -pdg-print -pdg-verbose 2
COMMENT: The two PDG tests above test interesting case where the slicing may
COMMENT: slice away a goto because of an incorrect analyze of some dead code,
COMMENT: which make the slicer think that the destination of the goto is the
COMMENT: syntactic successor of the goto instruction...
 **/


int G1;
void f1 (int c) {
  for (int x = 0; x < 10; x++) {
    G1 = 3;
    if (G1) break;
    return;
  }
}

int G1b;
void f1b (void) {
W: { {
    G1b = 3;
    if (G1b) goto B;
    return;
   }
   goto W;
   }
B: ;
}

int G2;
void f2(void) { 
  while (1) {
    G2 = 3;
    if (G2) break;
  }
}

int bts181 (int c) {
  int x = 0, y = 0;
  if (c) {
    x = 1;
    if (x>0) 
      y = 3;
    }
  return y;
}
int bts181b (int c) {
  int x = 0, y = 0;
  if (c) {
    x = 1;
    if (x>0) 
      y = 3;
    else
      y = 4;
    }
  return y;
}

int bts807 (void) {
  int g = 0;
  int b = 7;
  int a = 2;
  if ((( a || 42) && b)) {
    while (1) {
      g = 21;
      return g;
    }
  }
  return g;
}

int bts809 (void) {
  int x;
  while (1) {
    x = 10;
    goto L;
    while (x) {
L: return x;
    }
  }
}

// TODO: see COMMENT above.
int bts879 (int c) {
  int g = 0;
  int p = c ? 0 : 10;

  if (p  || (g && G1) ) { 
    return 1;
  }
  return 0;
}

// This one looks similar to the previous one, but without the block,
// Cil doesn't generate a goto from the then branch to the else branch...
int bts879b (int c) {
  int g = 0;
  int p = c ? 0 : 10;

  if (p || (g && G1) ) 
    return 1;
  
  return 0;
}

int one_time_loop_with_break () {
  int x;
  while (1) {
    x = 3;
    if (x > 0) break;
    x++;
    }
  return x;
}

/* TODO: find an example... I didn't manage to build one.
int one_time_loop_with_continue () {
  int x = 0;
  while (1) {
    x++;
    if (x == 2) break;
    if (x == 1) continue;
    }
  return x;
}
*/

int bts899 (void ) {
  int vrai = 1;
  int x = 254;
  for (int i = 17; (i != (-9)); i--) {
    if (! i) {
      if (vrai)
	continue;
      continue; // unreachable but disturb ctrl dependencies...
    }
    x ++;
  }
  return x;
}

int bts906 (void) {
  int x = 0;
  int i = 2;
  while (i >= 0) {
    while (1) {
      if (i)
        goto B;
      else {
        x ++;
        return x;
        if (x)
          goto B;
      }
    }
B : i --;
  }
  return 0;
}

int bts906b (void) {
  int x = 0;
  int i = 2;
  while (i >= 0) {
    while (1) {
      if (i)
        goto B;
      else {
        x ++;
        return x;
	x++;
        if (x)
          goto B;
      }
    }
B : i --;
  }
  return 0;
}


int bts906c (void) {
  int x = 0;
  int i = 2;
  while (i >= 0) {
    while (1) {
      if (i)
        goto B;
      else {
        x ++;
        return x;
      W:
	x++;
	goto W;
      }
    }
B : i --;
  }
  return 0;
}


int bts963 (void) {
   int x = 0;
   int i;
L: i = 0;
   while (i < 10) {
     x++;
     if (x < 3) goto L; 
     else return x;
   }
   return x;
}

int bts963b (void) {
   int x = 0;
   int i;
L: i = 0;
   while (i < 10) {
     x++;
     if (x < 3) goto L; 
     else return x;
     i++;
   }
   return x;
}

int main (int n) {
  int x = 0;
  f1 (n); x += G1;
  f1b (); x += G1b;
  f2 (); x += G2;
  x += bts181 (n);
  x += bts181b (n);
  x += bts807 ();
  x += bts809 ();
  x += bts879 (n);
  x += bts879b (n);
  x += bts899 ();
  x += bts906 ();
  x += bts906b ();
  //  x += bts906c ();
  x += bts963 ();
  x += bts963b ();
  return x;
}

