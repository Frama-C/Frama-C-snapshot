// invalid flexible array member (two incomplete fields in same field group)
struct {
  int len;
  char data[], more_data[];
} s;
