#include <stdlib.h>

typedef char *sds;

struct sdshdr8 {
    char len; /* used */
    char alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char buf[];
};

void sdslen(const sds s) {
    ((struct sdshdr8 *)((s)-(sizeof(struct sdshdr8))))->len; // should be OK
}

typedef struct {
  int i;
} st;

void main() {
  st *res = malloc(sizeof(struct inexistent)); // should be an error
}
