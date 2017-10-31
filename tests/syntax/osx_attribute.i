void f(void) __attribute__((availability(macos,introduced=10.4,deprecated=10.6,obsoleted=10.7)));

int main () { f(); }
