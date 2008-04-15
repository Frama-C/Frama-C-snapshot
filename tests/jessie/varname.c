
char* f();
char* ff();

void g() {
  if (ff())
  {
    if (ff()) return;
    if (!ff()) return;
  }
}

void h() {
  if (f())
  {
    if (f()) return;
    if (!f()) return;
  }
}
