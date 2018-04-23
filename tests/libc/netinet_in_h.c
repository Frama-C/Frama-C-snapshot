#include <netinet/in.h>
#include <stdio.h>

int main() {
  struct in_addr addr = {0};
  printf("%s", inet_ntoa(addr));
}
