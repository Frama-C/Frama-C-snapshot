/*run.config
  STDOPT: #"-val-split-return auto -slevel 2"
*/
// Extract based on Linux Programmer's Manual, GETADDRINFO(3) man page
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include "netdb.c"

#define BUF_SIZE 500

// Non-POSIX
#ifndef NI_MAXHOST
# define NI_MAXHOST 1025
#endif
#ifndef NI_MAXSERV
# define NI_MAXSERV 32
#endif

int main() {
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  int sfd, s;
  struct sockaddr_storage peer_addr;
  socklen_t peer_addr_len;
  ssize_t nread;
  char buf[BUF_SIZE];

  char *addr = "localhost";
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6 */
  hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
  hints.ai_flags = AI_PASSIVE;    /* For wildcard IP address */
  hints.ai_protocol = 0;          /* Any protocol */
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;

  s = getaddrinfo(NULL, addr, &hints, &result);
  if (s != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
    exit(EXIT_FAILURE);
  }

  /* getaddrinfo() returns a list of address structures.
     Try each address until we successfully bind(2).
     If socket(2) (or bind(2)) fails, we (close the socket
     and) try the next address. */

  for (rp = result; rp != NULL; rp = rp->ai_next) {
    sfd = socket(rp->ai_family, rp->ai_socktype,
                 rp->ai_protocol);
    if (sfd == -1)
      continue;

    if (bind(sfd, rp->ai_addr, rp->ai_addrlen) == 0)
      break;                  /* Success */

    close(sfd);
  }
  if (rp == NULL) {               /* No address succeeded */
    fprintf(stderr, "Could not bind\n");
    exit(EXIT_FAILURE);
  }

  freeaddrinfo(result);           /* No longer needed */

  return 0;
}
