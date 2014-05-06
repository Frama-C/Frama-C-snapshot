//@ axiomatic Foo { predicate p(char *s); }

void f(const char *__declspec(whatever) a, char* __declspec(p) b) {
  //@ assert p(b);
}
