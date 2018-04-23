// valid flexible array member declarations
struct {
  int size;
  char data[];
} s1;

struct {
  char len, data[];
} s2;
