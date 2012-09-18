/* run.config
   OPT: -val -lib-entry -context-width 4
*/

typedef struct {
  int f1;
  float f2;
  char f3; // padding
  char* f4;
  char f5; // trailing padding
} ts;

struct {
  char c1;
  ts tcs[10];
  char c2;
} s;

typedef struct {
  int f1;
  const int f2;
} ss;

typedef struct {
  double f1;
  double f2;
} ds;


int* t1[5000000];
int t2[5000000];
ts t3[1000];
char t4[5000000];
int* t5[3]; // test big context-width
ds t6[5000000];

void main () {
}
