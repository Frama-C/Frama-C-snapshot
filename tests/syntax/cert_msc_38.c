/* run.config
STDOPT: +"-cpp-extra-args='-DTEST_ASSERT'"
STDOPT: +"-cpp-extra-args='-DTEST_ERRNO'"
STDOPT: +"-cpp-extra-args='-DTEST_MATHERRHANDLING'"
STDOPT: +"-cpp-extra-args='-DTEST_VASTART'"
STDOPT: +"-cpp-extra-args='-DTEST_VACOPY'"
STDOPT: +"-cpp-extra-args='-DTEST_VAARG'"
STDOPT: +"-cpp-extra-args='-DTEST_VAEND'"
STDOPT: +"-cpp-extra-args='-DTEST_SETJMP'"
*/
#include <assert.h>
#include <stdarg.h>
#include <setjmp.h>

#ifdef TEST_ASSERT

typedef void (*handler_type)(int);

void execute_handler(handler_type handler, int value) {
  handler(value);
}

void func(int e) {
  // error: assert must be a macro, not a function
  execute_handler(&(assert), e < 0);
}

#endif

#ifdef TEST_ERRNO
// error: errno must be a macro
extern int errno;
#endif

#ifdef TEST_MATHERRHANDLING
// error math_errhandling must be a macro
extern int math_errhandling;
#endif

// error: can't suppress va_* macros
#ifdef TEST_VASTART
void *(*test1)() = &(va_start);
#endif

#ifdef TEST_VACOPY
void (*test2)() = &(va_copy);
#endif

#ifdef TEST_VAARG
void* (*test3)() = &(va_arg);
#endif

#ifdef TEST_VAEND
void (*test4)() = &(va_end);
#endif

// error can't suppress setjmp macro
#ifdef TEST_SETJMP
int (*test5)() = &(setjmp);
#endif
