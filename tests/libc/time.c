#include <sys/time.h>

void test_gettimeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, (void *) 0);
  /*@ assert( \initialized( &tv.tv_sec));  */
  /*@ assert( \initialized( &tv.tv_usec)); */
}

int main(int argc, char **argv)
{
  test_gettimeofday();
  return 0;
}


/*
Local Variables:
compile-command: "cd ../.. && ptests.byte -show -config gcc tests/libc/time.c"
End:
*/
