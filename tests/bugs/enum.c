enum fixed_addresses {
 A,
 B = -1UL,
 BASE,
 END = BASE,
};


enum e f(void) {
  return A;
}
