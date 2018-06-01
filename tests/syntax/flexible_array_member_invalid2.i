// invalid flexible array member (two incomplete fields)
struct {
  int len;
  char data[];
  char more_data[];
} s;
