// In C99, flexible array members cannot be nested inside other structs
typedef struct {
  int a;
  char data[];
} fam;

struct {
  int len;
  fam f;
} st;
