/* run.config*
STDOPT: #"-unspecified-access"
*/
struct S { int a; int b; };

int main () {
  struct S s;
  s.a = 0;
  s.b = 1;
  s.a = s.b = 2;
  return 0;
}
