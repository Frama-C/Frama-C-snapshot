int main() {
  int G = 258+128;
  signed char c;
  c = (signed char)G; // -126
  G = c;
  printf("%d\n",G);
  return G;
}
