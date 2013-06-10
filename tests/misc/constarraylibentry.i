/* run.config
   OPT: -val -lib-entry
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

void main(){
  int *x = t7;
}
