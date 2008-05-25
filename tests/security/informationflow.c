/* run.config
   GCC:
   OPT: -security-analysis
   */

/**********************/
/* High-security I/O. */
/**********************/

int read_high(void) {
  int n = 0;
  /* Input n from high channel... */
  return n;
}

void write_high(int n) {
  /* Output n to high channel... */
  return;
}

/*********************/
/* Low-security I/O. */
/*********************/

int read_low(void) {
  int n = 0;
  /* Input n from high channel... */
  return n;
}

void write_low(int n) {
  /* Output n to high channel... */
  return;
}

/**********************/
/* Various functions. */
/**********************/

int f1(int y) { int x; x = y; return(x); }
int f2(int y) { int x; x = y; return(x); }
int f3(int y) { int x; x = y; return(x); }
int f4(int y) { int x; x = y; return(x); }
int f5(int y) { int x; x = y; return(x); }
int f6(int y) { int x; x = y; return(x); }

/**********/
/* Cache. */
/**********/

int cache = 0;
void save(int y) { cache = y; }

/*********/
/* Main. */
/*********/

int main(void) {
  int hi, lo, *p, **q;
  void (*fptr)(int);
  char s[20];

  p = &hi;
  q = &p;
  fptr = write_low;

  /* 1. No flow from high channel to low channel. */
  hi = read_high();
  lo = read_low();
  write_high(f1(hi));
  write_low(f2(lo));
  write_high(f3(lo));
  write_low(cache);

  /* 2. Flow from high channel to low channel via data dependence. */
  hi = read_high();
  lo = read_low();
  lo = f4(lo);
  hi = f5(hi);
  lo = f6(lo);
  write_high(hi);
  write_low(lo);

  /* 3. Flow from high channel to low channel via control dependence. */
  hi = read_high();
  lo = 0;
  if (hi) {
    write_high(1);
    lo = 1;
    write_high(1);
  }
  write_low(lo);

  /* 4. Flow from high channel to low channel via global variable. */
  save(read_high());
  write_low(cache);

  /* 5. Flow from high channel to low channel via a pointer. */
  hi = read_high();
  write_low(*p);

  /* 6. Flow from high channel to low channel via a double pointer. */
  hi = read_high();
  write_low(**q);

  /* 7. Flow from high channel to low channel via a function indirection. */
  hi = read_high();
  fptr(hi);

  /* 8. Flow from high channel to low channel via pointer casting. */
/*   hi = read_high(); */
/*   p = (int *)hi; */
/*   write_low((int)*q); */

  /* 9. Flow from high channel to low channel through a string value.
      The dependence is sensitive to standard library models. */
/*   lo = 0; */
/*   hi = read_high(); */
/*   sprintf(s, "%d", hi); */
/*   sscanf(s, "%d", &lo); */
/*   write_low(lo); */

  return 0;
}
