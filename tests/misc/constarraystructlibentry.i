/* run.config
   OPT: -val -lib-entry -val-initialization-padding-globals yes -then -val-initialization-padding-globals no
*/

const int t[] = { 1, 2, 3, 4, 5 } ;

const int t2[3][3] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 } ;

typedef const int tt3[3];

tt3 t3[3] = { 10, 20, 30, 40, 50, 60, 70, 80, 90 } ;

typedef struct {
  int f1;
  const int f2;
} ss;

typedef struct {
  int f0;
  const char f2;
} ss2;

const int t4[12] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
ss t5[7] = {{1, 2}, {3}, 5, 6, 7, 8, 9, 10};
ss2 t6[6] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

extern const t7[5]; // Do not initialize to 0
extern const t8[5] = {1, 2}; // Ignore extern (done by Cil)

struct ts1 {int b; char c;};
struct ts2 {struct ts1 a; char c;};
struct ts3 {const struct ts1 a; char c;};
const struct ts1 s1;
const struct ts2 s2;
struct ts3 s3;

void main(){
  int *x = t7;
}
