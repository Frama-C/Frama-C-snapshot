/* run.config
STDOPT: +"-print"
*/

struct s {
  int a;
  char data[]; // FAM
};

int main() {
  struct s s1 = {0};
  return s1.a;
}
