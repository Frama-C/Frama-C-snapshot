int main() {
  char *s = "\\\\.\\";
  //@ assert s[0] == '\\';
  s[2] = '\\';
}
