// Tests related to flexible array members

// FAM declaration OK
struct {
  int len;
  char a[];
} fam;

// static FAM initialization not allowed (unsupported GCC extension)
struct {
  int len;
  char a[];
} fam2 = {1, {1, 2, 3, 4, 5, 6}};

// initialization OK - not a FAM
int t[] = {5, 6, 7, 8};

// initialization OK - not a FAM
int m[][3] = {{30, 31, 32},{33, 34, 35},{36, 37, 38}};

// invalid FAM declaration: b is not the last element in the struct
struct {
  char b[];
  int d;
} not_fam = {{1, 2, 3}, 4};

// OK (no FAMs)
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
} nested = {1, {2, {3, {4, 5}}}};

// static FAM initialization not allowed (unsupported GCC extension)
struct {
  int len;
  struct {
    int len2;
    char d[3];
  } not_a_fam;
  char a[];
} fam3 = {1, {2, {3, 4, 5}}, {6, 7, 8, 9, 10}};

typedef struct {
  int len; int a[];
} td_fam;
// static FAM initialization (via a typedef) not allowed
// (unsupported GCC extension)
td_fam tfam = {1, {1, 2, 3, 4, 5, 6}};

// OK - not a FAM
struct {
  int len;
  struct {
    int len2;
    char d[3];
  } not_a_fam;
} not_fam2 = {1, {2, {3, 4, 5}}};

int main() {
  // invalid: cannot initialize a FAM in a non-static context
  // (not even GCC allows this)
  struct {
    int len3;
    struct {
      int len4;
      char e[3];
    } not_a_fam2;
    char a[];
  } fam4 = {10, {11, {12, 13, 14}}, {15, 16, 17, 18, 19}};
  return 0;
}
