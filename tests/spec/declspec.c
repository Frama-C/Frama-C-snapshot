//@ axiomatic Foo { predicate p(char *s); }

void f(const char *__declspec(whatever) a) {
  //@ assert p(a);
}
