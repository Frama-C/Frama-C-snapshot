char* foo(char* dest, char* src, int s) {
  char* cur = dest;
  if (dest == 0) return 0;
  while (s-- > 0 && *src) {
    *cur++ = *src++;
  }
  *cur = '\0';
  return dest;
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int -d motivating.c" */
/* End: */
