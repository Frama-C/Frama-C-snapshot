int __builtin_constant_p(int a) { return a; }

void main() {
  __builtin_constant_p(1==1 && 1 || (1 & 1));
}
