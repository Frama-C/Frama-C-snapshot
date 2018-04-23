#include <sys/time.h>
#include <time.h>

void test_gettimeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, (void *) 0);
  /*@ assert( \initialized( &tv.tv_sec));  */
  /*@ assert( \initialized( &tv.tv_usec)); */
}

void test_strftime(void)
{
  char outstr[200];
  time_t t;
  struct tm *tmp;
  size_t res;
  t = time(NULL);
  tmp = localtime(&t);
  if (tmp) {
    res = strftime(outstr, sizeof(outstr), "%a %H %j %m %+ %% %Z", tmp);
  }
}

int main(int argc, char **argv)
{
  test_gettimeofday();
  test_strftime();
  return 0;
}


/*
Local Variables:
compile-command: "cd ../.. && ptests.byte -show -config gcc tests/libc/time.c"
End:
*/
