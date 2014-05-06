/* run.config
   STDOPT: +"-input -out -inout -then -plevel 250 -then -wlevel 56"
*/

struct s {
  int f1[5];
  int f_inter[5];
  char f2;
  int f_inter2[5];
};

struct s t[50];

int ct() {
  return 20;
}

//@ requires i != 157; assigns \nothing;
void f(int i);

//@ requires i != 158; assigns \nothing;
void g(int i);

int i, j;
int q, r;

int main(int v) {
  for (i=0; i<5; i++) {
    for (j=0; j<50; j++) {
      t[j].f1[i] = 10;
    }
  }
  Frama_C_dump_each();
  for (j=0; j<50; j++) {
    t[j].f2 = 157;
  }
  for (i=0; i<5; i++) {
    for (j=0; j<50; j++) {
      t[j].f1[i] = ct();
    }
  }
  Frama_C_dump_each();

  for (i=0; i<5; i++) {
    for (j=0; j<50; j++) {
      r = t[j].f1[i]+1;
      q = t[j].f1[i];
      f(t[j].f1[i]);
      g(t[j].f1[i]+1);
    }
  }
  return q+r;

}
