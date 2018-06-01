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

int main() {
  return g2.f5;
}
