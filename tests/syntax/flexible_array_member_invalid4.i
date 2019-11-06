// invalid flexible array member (incomplete field is not last)
struct s {
  int len;
  char data[];
  char b;
} ss;
