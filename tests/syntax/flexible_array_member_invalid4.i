// invalid flexible array member (incomplete field is not last)
struct {
  int len;
  char data[];
  char b;
} s;
