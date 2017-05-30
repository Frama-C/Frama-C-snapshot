int main() {
  int i = 1;
  i = (i < 2 ? ++i : i++);

  if (i < 2) { ++i; i = i; } else { int tmp = i; i++; i=tmp; }
  return i;
}
