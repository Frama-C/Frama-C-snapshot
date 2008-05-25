typedef unsigned int size_t;
#define NULL ((void*)0)
#define HEAP_SIZE 1000
struct EMPTY_STRUCT MALLOCED_HEAP;
void*malloc(size_t s) {
  static int next_free=0;
  void * new = ((char*)(&MALLOCED_HEAP))+next_free;
  next_free += s;
// To enforce a 4 byte alignement replace with
//   next_free += (s%4==0)?s:s+4-s%4;
// To analyze out of memory situations enable this:
//  if (next_free>=HEAP_SIZE) return NULL;
 return new;
}
void*realloc(void*ptr,size_t s) {
  if (s==0) return NULL;
  return (malloc(s));
}
void*alloca(size_t s) {return malloc(s);}
void free (void * ptr) { return; }
void*calloc (size_t nmemb, size_t size){
  int octets = size*nmemb;
  int counter;
  void *r = malloc(octets);
  for (counter=0;counter<=octets-1;octets++) ((char*)r)[counter]=0;
  return r;
};
