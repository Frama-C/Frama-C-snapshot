char *strcat(char * dest, const char *src) {
  char *ret = dest;
  while( *dest ) { dest++; }
  while( *dest++ = *src++ );
  return ret;
}

int main() {
  char str[2];
  str[0] = '\0';
  strcat(str,"/");
  return 0;
}

/*
Local Variables:
compile-command: "LC_ALL=C make bts6468"
End:
*/

