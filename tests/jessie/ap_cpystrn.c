#define EOS 0

typedef unsigned int size_t;

/* Rewritten to be more analyzable -- use explicit array indexing. */
char * ap_cpystrn(char *dst, const char *FRAMA_C_STRING src, size_t dst_size)
{
  int i;

  if (dst_size == 0)
    return (dst);
  
  for (i = 0; i < dst_size - 1; i++) {
    dst[i] = src[i];
    if (src[i] == EOS) {
      return dst + i;
    }
  }

  dst[i] = EOS;

  return dst + i;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make ap_cpystrn"
End:
*/

