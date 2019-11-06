// In C99, flexible array members cannot be nested inside other structs
typedef struct {
  int a;
  char data[];
} fam;

struct st {
  int len;
  fam f;
} sst;
