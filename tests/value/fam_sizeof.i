// Tests related to flexible array members

struct {
  int len;
  char a[];
} fam;

struct _nested {
  int level1;
  struct _s1 {
    int level2;
    struct _s2 {
      int level3;
      struct _s3 {
	int level4;
	int another4;
      } s3;
    } s2;
  } s1;
} nested;

typedef struct {
  int len;
  struct {
    int len2;
    char d[3];
  } not_a_fam;
} not_fam2;

struct {
  int len;
  not_fam2 n;
  not_fam2 n2[];
} fam2;

// GCC-style FAMs allow operator sizeof, which must return 0
struct {
  int len;
  char fam[0];
} gcc_fam;

int main() {
  unsigned long z1 = sizeof(fam);
  //@assert z1 == sizeof(int);
  unsigned long z2 = sizeof(nested);
  unsigned long z3 = sizeof(not_fam2);
  unsigned long z4 = sizeof(fam2);
  //@assert z4 == sizeof(int) + sizeof(not_fam2);
  unsigned z5 = sizeof(gcc_fam.fam);
  //@assert z5 == 0;
  return 0;
}
