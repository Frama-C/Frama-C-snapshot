int main() {
  int i = (signed char)256  ? 42 : 36; // UB prior simplification.
  int j = (unsigned char)256  ? 42 : 36;
}
