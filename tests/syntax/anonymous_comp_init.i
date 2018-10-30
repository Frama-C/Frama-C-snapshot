/*run.config
  STDOPT: #"-c11"
*/

typedef struct {
  int f1;
  struct {
    int f2;
  };
  int f3;
} s1;

typedef struct {
  union {
    struct {
      unsigned f1: 1;
      unsigned f2: 1;
      unsigned f3: 1;
    unsigned : 10;
      unsigned f5: 1;
    unsigned : 6;
    };
    unsigned i;
  };
  unsigned j;
} s2;

typedef struct {
   union {
      int a;
      long b;
   };
} s3;

s1 g1 = {
  .f1 = 1,
  .f3 = 3
};

s2 g2 = {
  .f1 = 1,
  .f2 = 1,
  .f3 = 0,
  .f5 = 1,
  .j = 0
};

s2 g3 = {
  .f3 = 1,
  2,
  .f1 = 3,
  4
};

s2 g4 = { { { .f1 = 0 } } };
s2 g5 = { 1, 2, 3, 4, 5 };
s2 g6 = { { 1, 2, 3, 4}, 5 };

/* According to the braces, the 5 correspond to i instead of j, which cause the
   5 to be ignored (too many elements in union) while j has no special value
   (initialized to 0) */
s2 g7 = { { { {1}, 2, 3, 4}, 5} };

s3 g8 = { { .a = 0 } };

int main() {
  return g2.f5;
}


