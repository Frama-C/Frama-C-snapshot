/*run.config*
  STDOPT: +"-eva-no-builtins-auto"
*/

#define NULL ((void*)0)

char * strchr(const char * s, int c)
{  for(; *s != (char) c; ++s)
    if (*s == '\0')
      return NULL;
  return (char *) s;
}

char s1[10]={'a','b','a','b','a','b','a','b','a','b'};
char *x;

void main(void)
{
  x = strchr(s1, 'c');
}
