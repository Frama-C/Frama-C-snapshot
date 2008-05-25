/* run.config
   DONTRUN: no annotation here!
*/
/*****************************************************************************/
/* Attempt to define a running example for ACSL (Ansi C Specification        */
/* Language), much as the Purse example in JML description papers.           */
/* The goal is to exercise as much as possible of ACSL.                      */
/*****************************************************************************/

#define N 10000

enum error_tag {
  MARSHALL_ERROR = 1,
  ALLOC_ERROR = 2,
  MESSAGE_ERROR = 3,
};

/* Allocation */

static char heap[N];

static char *pos = heap;
static char *end = heap + N;

char* alloc(unsigned int n) {
  char *cur = pos;
  char *next = pos + n;
  if (next > end) return 0;
  pos = next;
  return cur;
}

/* Strings */

unsigned int strlen(char *s) {
  unsigned int size = 0;
  while (*s++ != '\0') { size++; }
  return size;
}

char* strcpy(char* dest, char* src) {
  char *cur = dest;
  while (*src != '\0') { *cur++ = *src++; }
  return dest;
}

/* Marshalling */

enum marshall_tag {
  MARSHALL_INT = 1,
  MARSHALL_STRING = 2,
};

char *marshall_int(char* p, int i) {
  char *dest = p;
  char *src = (char*)&i;
  char *end;
  *dest++ = MARSHALL_INT;
  end = dest + sizeof(int);
  while (dest < end) {
    *dest++ = *src++;
  }
  return dest;
}

char *marshall_string(char* p, char* s) {
  char *dest = p;
  char *src = s;
  *dest++ = MARSHALL_STRING;
  strcpy(dest,src);
  return dest;
}

char* unmarshall_int(char* p, int* i) {
  char *src = p;
  char *dest = (char*)i;
  char *end;
  if (*src != MARSHALL_INT) return 0;
  src++;
  end = dest + sizeof(int);
  while (dest < end) {
    *dest++ = *src++;
  }
  return src;
}

char* unmarshall_string(char* p, char** s) {
  char *src = p;
  char *dest;
  int size;
  if (*src != MARSHALL_STRING) return 0;
  src++;
  size = strlen(src);
  *s = alloc(size + 1);
  if (s == 0) return 0;
  dest = *s;
  strcpy(dest,src);
  src += size + 1;
  return src;
}

/* Messages */

struct Msg {
  int   level;
  char* text;
};

char* msg_create(struct Msg* s) {
  char *msg = alloc(2 + sizeof(int) + strlen(s->text) + 1);
  char *p = msg;
  if (p == 0) return 0;
  p = marshall_int(p,s->level);
  if (p == 0) return 0;
  p = marshall_string(p,s->text);
  if (p == 0) return 0;
  return msg;
}

int msg_receive(char* p, struct Msg* s) {
  p = unmarshall_int(p,&s->level);
  if (p == 0) return MESSAGE_ERROR;
  p = unmarshall_string(p,&s->text);
  if (p == 0) return MESSAGE_ERROR;
  return 0;
}

/* Test */

int main(int argc, char** argv) {
  struct Msg m1;
  struct Msg m2;
  char *msg;
  int iter, status;
  for (iter = 0; iter < argc; iter++) {
    m1.level = iter;
    m1.text = argv[iter];
    msg = msg_create(&m1);
    if (msg == 0) return MESSAGE_ERROR;
    status = msg_receive(msg,&m2);
    if (status != 0) return MESSAGE_ERROR;
  }
  return 0;
}
