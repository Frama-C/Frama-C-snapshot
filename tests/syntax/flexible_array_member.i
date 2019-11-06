// valid flexible array member declarations
struct s1 {
  int size;
  char data[];
} ss1;

struct s2 {
  char len, data[];
} ss2;

union u {
  struct s {
    char len;
    char data[];
  } fam;
} u1;
