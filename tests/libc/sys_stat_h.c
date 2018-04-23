/*run.config
  STDOPT: #"-slevel 2"
*/
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main() {
  int fd = open("/tmp/bla", O_RDWR, S_IRWXU | S_IRWXG);
  if (fd == -1) return 1;
  if (close(fd)) return 2;
  struct stat st;
  int r = stat("/tmp/bla", &st);
  if (r) return r;
  if (st.st_size <= 0) return 3;
  return 0;
}
