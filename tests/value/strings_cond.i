
void foo(char const *s) {
  Frama_C_dump_each();
  while(*s) {   Frama_C_show_each_s(s); s++; }
}

void main(void) {
  foo("Bla");
}
