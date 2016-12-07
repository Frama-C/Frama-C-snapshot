/* run.config*
   STDOPT: +"-slevel 2"
*/
#include <termios.h>
#include <fcntl.h>

int main() {
  int fd;
  struct termios tio;
  fd = open("/dev/ttyS1", O_RDWR);
  int res = tcgetattr(fd, &tio);
  if (res) return 1;
  if (tio.c_cflag | CS8) {
    return 8;
  }
  tio.c_lflag = (ECHO|ICANON|ISIG|ECHOE|ECHOK|ECHONL);
  tio.c_oflag = OPOST;
  return tcsetattr(fd, TCSADRAIN, &tio);
}
