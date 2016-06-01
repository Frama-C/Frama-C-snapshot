typedef void *va_list;

#define va_start(AP, LASTARG) \
 (AP = ((va_list) __builtin_next_arg (LASTARG)))


void main(const char *pszMessage,...) {
  va_list vlParameters;
  va_start(vlParameters,pszMessage);
}
