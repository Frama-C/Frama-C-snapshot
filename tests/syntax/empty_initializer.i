/* run.config
STDOPT:+"-machdep gcc_x86_32"
*/
typedef struct __S { int i; } STR;

STR A[] = {
    { }, { }, { }
};

STR D[] = {
    { }, { 1 }, { }
};


int E[][3] = { { }, { } };

int f () {
  STR B[] = { { }, { }, { } };
  STR C[] = { { }, { 3 }, { } };
  int F[][4] = { { }, { 23, 45 }, { } };
  return B[1].i;
}

