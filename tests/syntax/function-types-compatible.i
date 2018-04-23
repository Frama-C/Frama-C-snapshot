void (*p)(int, ...);
void f();
void main() {
  p = f;
  p(1, 2);
}
