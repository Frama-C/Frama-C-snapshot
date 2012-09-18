/* run.config
   DONTRUN: syntactically incorrect include + no spec here...
*/
#ifndef PTEST
#include <stdio.h>
#else
extern int printf (__const char *__restrict __format, ...);
#endif

#ifndef PTEST
#include <setjmp.h>
#else
typedef int __jmp_buf[6];
typedef struct {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
typedef struct __jmp_buf_tag {
    __jmp_buf __jmpbuf;
    int __mask_was_saved;
    __sigset_t __saved_mask;
  } jmp_buf[1];

extern int _setjmp (struct __jmp_buf_tag __env[1]) __attribute__ ((__nothrow__));
extern void longjmp (struct __jmp_buf_tag __env[1], int __val)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
#define setjmp(env) _setjmp(env)
#endif


jmp_buf buf;
int previous_setjmp = 0 ;

void f(void) {
   longjmp(buf, 1);
}

int main(void) {
  previous_setjmp = -1 ;
  int setjmp_result = setjmp(buf);
  if (setjmp_result != 0) {
    printf("longjmp: setjmp_result=%d  previous_setjmp=%d\n",
           setjmp_result, previous_setjmp);
    return 0;
  }
  previous_setjmp = setjmp_result ;
  f();
  return 1;
}
