/* run.config*
   STDOPT: +"-load-module report -report -warn-signed-downcast -lib-entry -print -then -no-warn-signed-downcast -warn-unsigned-downcast -then -no-warn-unsigned-downcast -val-warn-signed-converted-downcast -then -main main5_wrap_signed -slevel 2 -no-print"
*/

signed char sx,sy,sz;
unsigned char uc;
int x;
unsigned int ux, uy,uz;
unsigned short s;

struct s {
  int i: 5;
  unsigned j: 5;
};

volatile v;

void main1(void) {
  sz = sx + sy;
  uc = sx + sy;
  uc = x;
  x = uy + uz;
  ux = uy + uz;
  s = uy + uz;
}

void main2_bitfield() {
  int i = 117;
  unsigned j = 254;

  struct s ss;
  if (v) ss.i = i;
  if (v) ss.j = j;
}

void main3_reduction() {
  int x = v;
  char c = x;

  unsigned int y = v;
  unsigned char d = y;
}

/* The cvalue abstraction does not represent how an address is represented in a
   C type. Thus alarms should always be emitted on a downcast of pointer values,
   as we don't known if they fit in the destination type. */
void main4_pointer() {
  int x;
  long long int p = (long long int)(&x);
  p += 100;
  unsigned int q = p;
  signed int r = p;
}

// Perform a computation that overflows on signed integers without alarm. The assertions can be proven with enough slevel
void main5_wrap_signed() {
  int x = v;
  //@ assert ASSUME: x >= 100000;
  //@ assert x > 0x7FFFFFFF-145 || x <= 0x7FFFFFFF-145; 
  unsigned int y = x;
  y += 145;
  int z = y;
  Frama_C_show_each(x, y, z);
  //@ assert z >= 100000 + 145 || z <= (int)(0x7FFFFFFF+145);
}


/* Tests for the relaxed downcast semantics -val-warn-signed-converted-downcast */
void main6_val_warn_converted_signed() {
  if (v) {
    short s = 65300u; // warning (overflow)
  }
  if (v) {
    unsigned short u = 65300u; // No warning
  }
  if (v) {
    unsigned long e = 17;
    short b = (short)e; // No warning, as 17 fits in short
  }
  if (v) {
    unsigned long e = -12; // No warning on unsigned casts
    short b = (short)e; // No warning, as -12 fits in short
                        // (but warning in -warn-signed-downcast mode)
  }
  if (v) {
    unsigned int e = -64000; // No warning on unsigned casts
    short b = (short)e; // Warning, as -64000 does not fit in short
  }
  if (v) {
    int *p = &v;
    int x = p; // No warning as an address fits in an integer.
    short y = p; // Warnings, as an address may not fit in short.
    unsigned short z = p; // No warninng on unsigned casts.
  }
}

void main7_signed_upcast() {
  unsigned char c= 240; // NEVER convert c to signed char
  int i = (int)c;
}

struct bitf {
  unsigned int i1: 18;
  signed int i2: 6;
};

/* Tests for the relaxed downcast semantics -val-warn-signed-converted-downcast with bitfields */
void main8_bitfields() {
  struct bitf S;
  signed char c;

  S.i1 = 0x3FFFF; // -1;

  if (v) S.i2 = S.i1;
  if (v) c = S.i1;

  S.i1 = 257u;

  if (v) S.i2 = S.i1; // Red
  if (v) c = S.i1; // Red

  S.i1 = 65u;

  if (v) S.i2 = S.i1; // Red
  if (v) c = S.i1;

}

void main9_bitfield () {
  struct { unsigned int a:11; } bf;
  bf.a = 1648; // -400 as a signed value of 11 bits
  if (v) {
    int signed_a = (int __attribute__((__FRAMA_C_BITFIELD_SIZE__(11))))bf.a;
    //@ assert signed_a == -400;
  }
  signed char c;
  if (v) c = bf.a;
}

// Check that we create only one alarm, even if we invent new expressions
void main10_loop () {
  signed char c;
  struct { unsigned int b:10; } bf;

  for (int k=0; k <10; k++) {
    bf.b = v;
    if (v) c = bf.b;
  }
}

void main() {
  main1();
  main2_bitfield();
  main3_reduction();
  main4_pointer();
  main5_wrap_signed();
  main6_val_warn_converted_signed();
  main7_signed_upcast();
  main8_bitfields();
  main9_bitfield();
  main10_loop();
}
