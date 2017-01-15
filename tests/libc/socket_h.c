#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>

int main() {
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) exit(1);
  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr("127.0.0.1");
  addr.sin_port = htons(42);
  int rc = connect(sockfd, (struct sockaddr *)&addr, sizeof(addr));
  if (rc < 0) exit(2);

  int optval;
  socklen_t optlen = sizeof(optval);
  rc = getsockopt(sockfd, SOL_SOCKET, SO_ERROR, (void *)&optval, &optlen);
  return rc;
}
