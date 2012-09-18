/* run.config
   OPT: -check -slice-return main -journal-disable -then-on 'Slicing export' -print
 **/
/* TESTS: this is about [filter] optimisations since sometimes, 
 * slicing results are ok, but the generated new project is not correct. */
int f(int);

int T[10];

/* When removing branches, one should take care about local variables. */
int bts806 () {
  int c = 0;
  int x = 0;

  if (c) {
    int y;
    { y = x+1;
      x = y;
    }
  }
  else {
    int z;
    { z = x+1;
      x = z;
    }
  }
  return x;
}


int unspec () {
  int c = 0;
  if (c)
    T[1] += f (T[1]);
  else
    T[2] += f (T[2]);
  return T[1] + T[2];
}

int main (int c) {
  int r = 0;
  r += bts806 ();
  r += unspec ();
  return r;
}
