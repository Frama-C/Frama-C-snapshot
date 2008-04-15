
typedef unsigned int size_t;

// 7.21.2.1: memcpy

char *memcpy(char *s1, const char *s2, size_t n) {
  char *r = s1;
  while (n-- > 0) *s1++ = *s2++;
  return r;
}

// 7.21.2.2: memmove

char *memmove(char *s1, const char *s2, size_t n) {
  char *tmp;
  if (n < 1) return s1;
  tmp = (char*)malloc(n * sizeof(char));
  memcpy(tmp,s2,n);
  memcpy(s1,tmp,n);
  return s1;
}

// 7.21.2.3: strcpy

char *strcpy(char *s1, const char *s2) {
  char *r = s1;
  while (*s1++ = *s2++) ;
  return r;
}

// 7.21.2.4: strncpy

char* strncpy(char *s1, const char *s2, size_t n) {
  char *r = s1;
  while ((n-- > 0) && (*s1++ = *s2++)) ;
  while (n-- > 0) *s1++ = 0;
  return r;
}

// 7.21.4.1: memcmp

int memcmp (const char *s1, const char *s2, size_t n) {
  int c;
  while (n-- > 0) {
    if ((c = *s1++ - *s2++) != 0) return c;
  }
  return 0;
}

// 7.21.4.2: strcmp

int strcmp (const char *s1, const char *s2) {
  int c;
  while (*s1 && *s2) {
    c = *s1++ - *s2++;
    if (c != 0) return c;
  }
  c = *s1 - *s2;
  return c;
}

// 7.21.4.3: strcoll

int strcoll (const char *s1, const char *s2) {
  return strcmp(s1,s2);
}

// 7.21.4.4: strncmp

int strncmp (const char *s1, const char *s2, size_t n) {
  int c = 0;
  while (*s1 && *s2 && (n-- > 0)) {
    c = *s1++ - *s2++;
    if (c != 0) return c;
  }
  if (n > 0) c = *s1 - *s2;
  return c;
}

// 7.21.4.4: strxfrm

size_t strxfrm (char *s1, const char *s2, size_t n) {
  size_t n1 = 0;
  while (*s2) {
    if (n-- > 0) *s1++ = *s2;
    s2++; n1++;
  }
  if (n > 0) *s1 = 0;
  return n1;
}

// 7.21.5.1: memchr

char *memchr (const char *s, char c, size_t n) {
  while (n-- > 0) {
    if (*s++ == c) return s-1;
  }
  return 0;
}

// 7.21.5.2: strchr

char *strchr (const char *s, char c) {
  while (*s) {
    if (*s++ == c) return s-1;
  }
  if (*s == c) return s;
  return 0;
}

// 7.21.5.3: strcspn
// postcondition added for strtok

/*@ ensures 0 <= \result <= \strlen(\old(s1)) */
size_t strcspn (const char *s1, const char *s2) {
  size_t n = 0;
  while (*s1 && strchr(s2,*s1++) == 0) ++n;
  return n;
}

// 7.21.5.4: strpbrk

char *strpbrk (const char *s1, const char *s2) {
  while (*s1) {
    if (strchr(s2,*s1) == 0) ++s1;
    else return s1;
  }
  return 0;
}

// 7.21.5.5: strrchr

char *strrchr(const char *s, char c) {
  char *r = 0;
  while (*s) {
    if (*s++ == c) r = s-1;
  }
  if (c == 0) r = s;
  return r;
}

// 7.21.5.6: strspn
// postcondition added for strtok

/*@ ensures 0 <= \result <= \strlen(\old(s1)) */
size_t strspn (const char *s1, const char *s2) {
  size_t n = 0;
  while (*s1 && strchr(s2,*s1++) != 0) ++n;
  return n;
}

// 7.21.5.7: strstr

char *strstr (const char *s1, const char *s2) {
  if (*s2 == 0) return s1;
  while (*s1) { 
    const char *rs1 = s1;
    const char *rs2 = s2;
    while (*rs1 && *rs2 && (*rs1 == *rs2)) { rs1++; rs2++; }
    if (*rs2 == 0) return s1;
    s1++;
  }
  return 0;
}

// 7.21.6.1: memset

char *memset (char *s, char c, size_t n) {
  char *r = s;
  while (n-- > 0) *s++ = c;
  return r;
}

// 7.21.6.3: strlen

size_t strlen(const char *s) {
  size_t n = 0;
  while (*s++) ++n;
  return n;
}


// Here we have gathered all 4 functions on which 
// inference does not work well, for 2 different reasons:
// - on strcat/strncat, precondition should include 
//   an inequality between 3 variables, which is not 
//   possible with only octagons
// - on strtok/strerror, precondition should mention 
//   global variables


// 7.21.3.1: strcat

/*@ requires \strlen(s1) >= 0 
  @       && \strlen(s2) >= 0
  @       && \arrlen(s1) > \strlen(s1) + \strlen(s2)
  @       && \full_separated(s1,s2)
*/
char *strcat (char *s1, const char *s2) {
  char *r = s1;
  /*@ invariant 0 <= s1 - \old(s1) <= \strlen(\old(s1)) */
  while (*s1) s1++;
  strcpy(s1,s2);
  return r;
}

// 7.21.3.2: strncat

/*@ requires \strlen(s1) >= 0 
  @       && \strlen(s2) >= 0
  @       && \arrlen(s1) >= \strlen(s1) + n 
  @       && \full_separated(s1,s2)
*/
char *strncat (char *s1, const char *s2, size_t n) {
  char *r = s1;
  /*@ invariant 0 <= s1 - \old(s1) <= \strlen(\old(s1)) */
  while (*s1) s1++;
  /*@ invariant s1 - \old(s1) == \old(\strlen(s1)) + \old(n) - n
    @        && 0 <= s1 - \old(s1)
    @        && 0 <= s2 - \old(s2) <= \strlen(\old(s2))
   */
  while ((n-- > 0) && (*s1++ = *s2++)) ;
  if (n >= 0) *s1 = 0;
  return r;
}

// 7.21.5.8: strtok

static char *tok;

/*@ requires (s1 != 0 => \strlen(s1) >= 0)
  @       && (s1 == 0 => \strlen(tok) >= 0)
  @       && \strlen(s2) >= 0
*/
char *strtok (char *s1, const char *s2) {
  char *r = 0;
  size_t n;
  if (s1 == 0) s1 = tok;
  s1 += strspn(s1,s2);
  if ((n = strcspn(s1,s2)) > 0) {
    r = s1;
    tok = s1 + n;
    if (*tok != 0) *tok++ = 0;
  }
  return r;
}

// 7.21.6.2: strerror

static char errbuf[1024];

extern char** errmsg;

/*@ requires 1 <= errnum <= \arrlen(errmsg)
  @       && \forall int i; 0 <= i < \arrlen(errmsg) 
  @          => (0 <= \strlen(errmsg[i]) < 1024 
  @              && \full_separated(errbuf, errmsg[i]))
 */
char *strerror (int errnum) {
  strcpy(errbuf,errmsg[errnum-1]);
  return errbuf;
}


/*
  (* Local Variables: *)
  (* compile-command: "caduceus --loc-alias --arith-mem --abs-int string_annot.c" *)
  (* End: *)
*/
