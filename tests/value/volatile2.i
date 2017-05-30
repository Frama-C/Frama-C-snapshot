/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,value -print -val @VALUECONFIG@ -machdep x86_16
*/


volatile unsigned char t[10];
struct u { unsigned char f1; unsigned char f2;};
volatile struct u u;

struct u * pu = &u; // Cast: remove volatile qualifier

void main1() {
  volatile unsigned char c = 1;

  int x = 1;
  volatile unsigned char *p = &x;
  /* The computation c << 8 overflows: c is promoted to _signed_ int,
     hence there is an overflow on 16 bits architecture. However, the
     entire computation does NOT overflow. c << 8 is NOT volatile (it
     is an expression), hence the last 8 bits are not set and the sum
     does not overflow. We check this for all kinds of lvalues, as
     they correspond to different branches of Cabs2cil. */
  unsigned int i = (c << 8) + c;
  unsigned int j = (*p << 8) + *p;
  unsigned int k = (t[1] << 8) + t[2];
  unsigned int l = (u.f1 << 8) + u.f2;
}

struct s {
  char i1;
  char i2;
} s;

volatile struct s *ps = &s; // Cast: add volatile qualifier

void main2() {
  // i and s are not volatile, but the access ps->i1 is.
  int i = ps->i1;
  int j = u.f1; // this field access is volatile
  int k = t[1];
}

volatile int v;

void main3() {
  if (v) {
    //@ assert \false;
  }

  if (v) {
    // Should be reachable: v must not be reduced by the 'if(v)'
    //@ assert \true;
  }

  //@ assert v == 0;
  //@ assert v == 0;
  if (v) {
    // Same
    //@ assert \true;
  }
}

void main4() {
  volatile int i;

  volatile int * p1 = &i; // No cast, &i has volatile qualifier
  volatile int * volatile p2 = &i; // No cast needed either. However, p2 itself
                                   // is also is volatile
}


void main5() {

  volatile int i = 0;
  volatile int j = 0;
  int k = i++ + j++;

  int l = ++i + ++j;

  Frama_C_dump_each();
}

void main6() {
  int i = 1;
  int j = (volatile int)i; // The cast can be dismissed: C99 6.5.4:4, note 86
}

struct bitf {
  int i: 3;
  unsigned j: 4;
};

volatile struct bitf BITF;

void main7() {
  int i = BITF.i + 1;
  int k = BITF.j + 1;
}

typedef struct {
  int field;
} S;
typedef volatile S vS;
typedef volatile struct {
  int field;
} vS2;

typedef union {
  int field;
} U;
typedef volatile U vU;

void main8() { // Test that volatile qualifiers hidden inside typedefs are taken into account
  volatile S a = 0;
  vS b = 0;
  vS2 c = 0;
  volatile U d = 0;
  vU e = 0;
  Frama_C_show_each(a, b, c, d, e);
}

extern struct { volatile int i1; int i2; } S9;
volatile int u9[10];

// Check that remove-redundant-alarms does not propagate information
// about predicates that involve volatile memory zones
void main9 () {
  int t9[10];
  t9[S9.i1] = 1;
  t9[S9.i1] = 2;
  t9[u9[1]] = 3;
  t9[u9[1]] = 4;
}

void main() {
  main1();
  main2();
  main3();
  main4();
  main5();
  main6();
  main7();
  main8();
  main9();
}
