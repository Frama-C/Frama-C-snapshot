/* run.config*
   STDOPT: +"-then -no-warn-signed-overflow -eva-no-warn-pointer-subtraction"
*/

struct ss {
  char *f1;
  int *f2;
  int f3;
};

struct s {
  struct ss f4;
  int f5;
};

volatile struct s *p;
struct s s2;

char x;
int y;

void main() {
  p = &s2;

  p->f4.f1 = &x+1;
  p->f4.f2 = &y-3;

  char *q1 = p->f4.f1;
  int *q2 = p->f4.f2;
  int i = p->f5;
  int j = (int) p->f4.f2;

  int r = (&x - p->f4.f1)+1;
  int s = (&y - p->f4.f2)+3;
}
