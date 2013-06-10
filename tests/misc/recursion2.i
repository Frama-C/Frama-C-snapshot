/*run.config
  OPT: -journal-disable -input -val -out -inout
 */
int x, y;

void h2 (int);
void h1 (int);

void h1 (int i) {
  int r = x;
  if (i)
    h2 (i);
}
void h2 (int j) {
  int q = y;
  if (!j)
    h1 (j);
}

void main() {
  h2(0);
  h1(1);
}
