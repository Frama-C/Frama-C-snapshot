/* run.config
   STDOPT: +"-machdep gcc_x86_32"
 */

#include "share/libc/stdint.h"

#define likely(x)  __builtin_expect((x),1)
#define unlikely(x)  __builtin_expect((x),0)

int16_t __sync_fetch_and_add_int16_t (int16_t *ptr, int16_t value,...)
{
  int16_t tmp = *ptr;
  *ptr += value;
  return tmp;
}
int16_t __sync_fetch_and_sub_int16_t (int16_t *ptr, int16_t value,...)
{
  int16_t tmp = *ptr;
  *ptr -= value;
  return tmp;
}

int32_t __sync_fetch_and_add_int32_t (int32_t *ptr, int32_t value,...)
{
  int32_t tmp = *ptr;
  *ptr += value;
  return tmp;
}

int32_t __sync_fetch_and_sub_int32_t (int32_t *ptr, int32_t value,...)
{
  int32_t tmp = *ptr;
  *ptr -= value;
  return tmp;
}

int64_t __sync_fetch_and_add_int64_t (int64_t *ptr, int64_t value,...)
{
  int64_t tmp = *ptr;
  *ptr += value;
  return tmp;
}

int64_t __sync_fetch_and_sub_int64_t (int64_t *ptr, int64_t value,...)
{
  int64_t tmp = *ptr;
  *ptr -= value;
  return tmp;
}

int16_t __sync_add_and_fetch_int16_t (int16_t *ptr, int16_t value,...)
{
  *ptr += value;
  return *ptr;
}

int16_t __sync_sub_and_fetch_int16_t (int16_t *ptr, int16_t value,...)
{
  *ptr -= value;
  return *ptr;
}

int32_t __sync_add_and_fetch_int32_t (int32_t *ptr, int32_t value,...)
{
  *ptr += value;
  return *ptr;
}

int32_t __sync_sub_and_fetch_int32_t (int32_t *ptr, int32_t value,...)
{
  *ptr -= value;
  return *ptr;
}

int64_t __sync_add_and_fetch_int64_t (int64_t *ptr, int64_t value,...)
{
  *ptr += value;
  return *ptr;
}

int64_t __sync_sub_and_fetch_int64_t (int64_t *ptr, int64_t value,...)
{
  *ptr -= value;
  return *ptr;
}

int __sync_bool_compare_and_swap_uint16_t (uint16_t *ptr, uint16_t oldval, uint16_t newval,...)
{
   if (*ptr == oldval) {
     *ptr = newval;
     return 1;
   } else {
     return 0;
   }
}

int __sync_bool_compare_and_swap_uint32_t (uint32_t *ptr, uint32_t oldval, uint32_t newval,...)
{
   if (*ptr == oldval) {
     *ptr = newval;
     return 1;
   } else {
     return 0;
   }
}

int __sync_bool_compare_and_swap_uint64_t (uint64_t *ptr, uint64_t oldval, uint64_t newval,...)
{
   if (*ptr == oldval) {
     *ptr = newval;
     return 1;
   } else {
     return 0;
   }
}

void main(void) {
  {
    int16_t content = 100;
    int16_t *ptr = &content;
    int16_t value = 33;
    int16_t result;
    result = __sync_fetch_and_add(ptr, value);
    /*@ assert result == 100 && content == 133; */
    result = __sync_fetch_and_add(ptr, -11);
    /*@ assert result == 133 && content == 122; */
    result = __sync_fetch_and_sub(ptr, value);
    /*@ assert result == 122 && content == 89; */
    result = __sync_fetch_and_sub(ptr, -11);
    /*@ assert result == 89 && content == 100; */
  }
  {
    int32_t content = 100;
    int32_t *ptr = &content;
    int32_t value = 33;
    int32_t result;
    result = __sync_fetch_and_add(ptr, value);
    /*@ assert result == 100 && content == 133; */
    result = __sync_fetch_and_add(ptr, -11);
    /*@ assert result == 133 && content == 122; */
    result = __sync_fetch_and_sub(ptr, value);
    /*@ assert result == 122 && content == 89; */
    result = __sync_fetch_and_sub(ptr, -11);
    /*@ assert result == 89 && content == 100; */
  }
  {
    int64_t content = 100;
    int64_t *ptr = &content;
    int64_t value = 33;
    int64_t result;
    result = __sync_fetch_and_add(ptr, value);
    /*@ assert result == 100 && content == 133; */
    result = __sync_fetch_and_add(ptr, -11);
    /*@ assert result == 133 && content == 122; */
    result = __sync_fetch_and_sub(ptr, value);
    /*@ assert result == 122 && content == 89; */
    result = __sync_fetch_and_sub(ptr, -11);
    /*@ assert result == 89 && content == 100; */
  }
  {
    uint16_t content = 100;
    uint16_t *ptr = &content;
    uint16_t oldval = 100;
    uint16_t newval = 133;
    int result;
    result = __sync_bool_compare_and_swap(ptr, oldval, newval);
    /*@ assert result == 1 && *ptr == newval; */
  }
  {
    uint32_t content = 100;
    uint32_t *ptr = &content;
    uint32_t oldval = 100;
    uint32_t newval = 133;
    int result;
    result = __sync_bool_compare_and_swap(ptr, oldval, newval);
    /*@ assert result == 1 && *ptr == newval; */
  }
  {
    uint64_t content = 100;
    uint64_t *ptr = &content;
    uint64_t oldval = 100;
    uint64_t newval = 133;
    int result;
    result = __sync_bool_compare_and_swap(ptr, oldval, newval);
    /*@ assert result == 1 && *ptr == newval; */
  }
  if (likely(4 == 4)) {
    int x = 1;
  }
  if (unlikely(3 == 4)) {
    int x = 0;
  }
  int x = 2;
  if (__builtin_expect(x++, x)) {
    int y = x;
  }
}
