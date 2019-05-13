/* run.config
OPT: -machdep x86_16 -print
OPT: -machdep x86_32 -print
OPT: -machdep x86_64 -print
OPT: -machdep gcc_x86_16 -print
OPT: -machdep gcc_x86_32 -print
OPT: -machdep gcc_x86_64 -print
OPT: -machdep ppc_32 -print
OPT: -machdep msvc_x86_64 -print
*/

#include <inttypes.h>
#include <stdio.h>

#define CHECK_SIGNED(kind,KIND,size) \
  { int##kind##size##_t x; \
    scanf("%"SCNd##KIND##size,&x); \
    printf("%"PRId##KIND##size,x); \
    scanf("%"SCNi##KIND##size,&x); \
    printf("%"PRIi##KIND##size,x); \
  }

#define CHECK_UNSIGNED(kind,KIND,size) \
 { uint##kind##size##_t x; \
   scanf("%"SCNo##KIND##size,&x); \
   printf("%"PRIo##KIND##size,x); \
   scanf("%"SCNu##KIND##size,&x); \
   printf("%"PRIu##KIND##size,x); \
   scanf("%"SCNx##KIND##size,&x); \
   printf("%"PRIx##KIND##size,x); \
   scanf("%"SCNx##KIND##size,&x); \
   printf("%"PRIX##KIND##size,x); \
 }

#define CHECK(kind,KIND,size) \
  CHECK_SIGNED(kind,KIND,size) \
  CHECK_UNSIGNED(kind,KIND,size)

#define CHECK_KIND(kind,KIND) \
  CHECK(kind,KIND,8) \
  CHECK(kind,KIND,16) \
  CHECK(kind,KIND,32) \
  CHECK(kind,KIND,64)

int main () {
  CHECK_KIND(,);
  CHECK_KIND(_least,LEAST);
  CHECK_KIND(_fast,FAST);
  CHECK(max,MAX,);
  CHECK(ptr,PTR,);
}
