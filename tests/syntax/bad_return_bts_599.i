
int BadReturn1(int* p) {
  *p++;
  return;
}

int BadReturn2(int* p) {
  *p++;
  return;
}


int main() {
  int i = 3;
  BadReturn2(&i);

  BadReturn1(&i);
  return 0;
}
