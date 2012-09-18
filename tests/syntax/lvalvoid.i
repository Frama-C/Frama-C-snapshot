void* memcpy1(void* dst, const void *src,long n) {
  char* d=dst; char* s=src;
  for (int i=0;i<n;i++)
    d[i]=(char)(src[i]);
  return dst;
}

void* memcpy2(void* dst, const void *src,long n) {
  char* d=dst; char* s=src;
  for (int i=0;i<n;i++)
    dst[i]=s[i];
  return dst;
}
