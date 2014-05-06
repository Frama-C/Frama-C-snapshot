struct S { int f; };

int main () {
  struct S s1 = { 0 };
  struct S s2, s3, s4,s5;
  s2.f = s3.f = s4.f = s5.f = s1.f;
  return s2.f;
}
