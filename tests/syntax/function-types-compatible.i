void (*p)(int, ...);
void f();
void main() {
  p = f;
  p(1, 2); // warning, but no parsing error; will fail during execution
}
