/* run.config
 OPT: -lib-entry -journal-disable -sparecode-debug 1 -sparecode-analysis
 OPT: -lib-entry -slice-pragma main -slice-return main -journal-disable -then-on 'Slicing export' -print
 OPT: -journal-disable -sparecode-debug 1 -rm-unused-globals
*/

// can be removed
int G1, G2;
int * PG1 = &G1;

// can be removed
typedef struct { int a; } Ts;
Ts Gts;
typedef Ts * Ps;
Ps GPs;

// Cannot be removed : used in spec
typedef struct { int a; int b; } Ts2;
Ts2 S2;


typedef char Ts2bis;
Ts2bis C = 'a';

// Can be removed : used in an unused function
typedef struct { int a; int b; int c; } Ts3;
Ts3 S3;

int f (void) {
  return S3.a + S3.b + S3.c;
}

typedef int Int;
typedef Int Tx;
char Size;
Tx X = sizeof (Size);
int Y;

int use_in_PX_init;
int * PX;

/*@ requires S2.a > S2.b ; */
int main (int x, Ts s) {
  //@ slice pragma expr S2 ;
  int y = 3;
  y += Y;
  y += *PX;
  //@ assert X > 0;
  return X + x;
}

int * PX = &use_in_PX_init;
