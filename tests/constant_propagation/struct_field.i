/* run.config
   OPT: -val -semantic-const-folding -journal-disable
*/

struct st {
  int a, b;
} ;

union u {
  int u1;
  struct st u2;
} ;

struct {
  int f0;
  int f1[9];
  union u f2[3];
} S;

/* specific test for union */
struct s1 {
  char c1;
  int c2;
};

struct s2 {
  int c3;
  char c4;
};

union v { 
  struct s1 f1;
  struct s2 f2;
};

union v v;

void main() {
  int *p1 = &S;
  int *p2 = p1 + 1;
  int *p6 = p2 + 4;
  int **pp = &p6;
  int *p11 = *pp+5;
  int *p14 = p11+3;
  char *p = (char *) &v + 4;
}
