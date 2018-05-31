// invalid field with function type, parsing should fail
struct {
  void f(int);
} s;
