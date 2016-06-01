struct my_unpacked_struct
{
  char c;
  int i;
};

struct my_packed_struct
{ char c;
  int  i;
  struct my_unpacked_struct s;
} __attribute__ ((__packed__));

struct my_packed_struct f(struct my_packed_struct foo) {
  struct my_packed_struct bar=foo;
  return foo;
}

struct my_packed_struct main(struct my_packed_struct foo) {
  f(foo);
  return foo;
}
