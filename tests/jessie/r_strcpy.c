
#define EOS 0

char *r_strcpy (char *dest, const char *src)
{
  int i;
  char tmp;
  for (i = 0; ; i++) {
    tmp = src[i];
    /* r_strcpy RELEVANT */
    dest[i] = tmp; // DO NOT CHANGE THE POSITION OF THIS LINE
    if (src[i] == EOS)
      break;
  }
  return dest;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make r_strcpy"
End:
*/
