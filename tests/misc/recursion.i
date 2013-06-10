/*run.config
  OPT: -lib-entry -main main -val -journal-disable
  OPT: -lib-entry -main main -val -val-ignore-recursive-calls -journal-disable
 */
int G;

int ff() {
  if (G) ff();
  return 5;
}

int x;

volatile int c;

struct s {
  int f1;
  int f2;
} s;

// Use given assigns
/*@ assigns x \from x, y;
  assigns s.f1 \from s.f2; 
  assigns \result \from s;
*/
struct s f(int y) {
  x = 2+y;
  Frama_C_show_each(x, y);
  if (c) {
    s = f(y);
    Frama_C_show_each(x, y);
  }
  s.f1 = s.f2;
  return s;
}

// Infers assigns \nothing
void g() {
  g();
}

// Infer assigns clause that overwrite *p1 and *p2
void h(int *p1, int *p2) {
  h(p1, p2);
}


int *pg;

/* &i escapes. The precondition is true on all calls, but could be computed
   false if one overwrites the value of i naively at each call */
/*@ requires stage > 0 ==> *pg == i-5;
  assigns *pg \from \nothing;
  ensures stage > 0 ==> *pg == 8;
*/
void escaping_formal(int stage, int i) {
  pg = &i;
  Frama_C_show_each (pg, *pg, stage, i);
  escaping_formal (1, i+5);
  if (stage > 0)
    *pg = 8;
  Frama_C_show_each (pg, *pg, stage, i);
  pg = 0;
}

int main() {
  G = ff();
  g();
  int v1, v2;
  h(&v1, &v2);
  Frama_C_show_each(v1, v2);
  escaping_formal(0, 10);
  struct s r = f(0);
  Frama_C_show_each(x);
  return r.f1+1;
}

