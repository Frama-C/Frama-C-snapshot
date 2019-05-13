/* run.config*
   STDOPT: #"-eva-no-builtins-auto"
*/
typedef unsigned int size_t;

void*malloc(size_t s);
void*realloc(void*ptr,size_t s);
void*alloca(size_t s);
void free (void * ptr);
void*calloc (size_t nmemb, size_t size);

void main(int test) {
  char * buf=0;
  if (test) buf = (char*)malloc(sizeof(char)*5);
  else buf = (char*)realloc(&test,sizeof(char)*6);
  
  if (test) buf[1] = 16;

  return;
}
