
unsigned long Frama_C_strnlen(char *, unsigned long);

char t1[5] = "abcde";
char t2[] = "abcde";

main(int c){
  int r1a, r1b, r1c, r2a, r2b, r2c;
  r1a = Frama_C_strnlen(t1, 3);
  r1b = Frama_C_strnlen(t1, 5);
  if (c & 1) r1c = Frama_C_strnlen(t1, 6);

  r2a = Frama_C_strnlen(t2, 3);
  r2b = Frama_C_strnlen(t2, 5);
  r2c = Frama_C_strnlen(t2, 6);

  Frama_C_dump_each();
}
