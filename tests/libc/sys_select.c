#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdlib.h>

int main() {
  int fd1 = socket(AF_INET, SOCK_DGRAM, 0);
  if (fd1 < 0) exit(1);
  int max_fd;

  fd_set fds1, fds2;

  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(8000);
  int res = bind(fd1, (struct sockaddr *)&addr, sizeof(addr));
  if (res < 0) exit(2);
  FD_ZERO(&fds1);
  max_fd = fd1;
  FD_SET(fd1, &fds1);

  struct timeval timeout;
  timeout.tv_sec = 20;
  timeout.tv_usec = 0;

  memcpy(&fds2, &fds1, sizeof(fds1));
  res = select(max_fd + 1, &fds2, NULL, NULL, &timeout);
  if (FD_ISSET(fd1, &fds2)) { // check that fds2 is initialized
    // ...
  }

  return 0;
}
