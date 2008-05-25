
void main() {
  char* FRAMA_C_STRING p = "done";
  int i = 0;
  while (p[i] != 0) i++;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make string_constant"
End:
*/
