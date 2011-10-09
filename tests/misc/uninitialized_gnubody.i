int main () {
  int x = 0;
  x = ({ int y = 0; y++; y + 1;}) + 42;
  return x;
}
