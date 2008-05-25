
struct S {unsigned char a; int b;};
const struct S T[2] = {{.a=1,.b=2},{.a=3,.b=4}};

unsigned short int main () {
  return (T[0].b);
}
