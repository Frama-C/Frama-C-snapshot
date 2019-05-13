/* run.config
   STDOPT: +"-eva-verbose 2 -eva-no-builtins-auto"
   STDOPT: +"-eva-verbose 2 -machdep x86_64 -eva-no-builtins-auto"
*/
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/uio.h>
#include <strings.h>
#include <netinet/in.h>

const char* sent_msg = "World";
#define SIZEOF_SENT_MSG 6
// char send_buffer[SIZEOF_SEND_BUFFER];

/* Contiguous receive buffer. */
#define SIZEOF_RCV_BUFFER 10
char rcv_buffer[SIZEOF_RCV_BUFFER];

/* Scattered receive buffer. Initialized locally so that it is UNINITIALIZED. */
#define DECLARE_SCATTERED_RECEIVE_BUFFER \
char rcv_buffer_scattered1[2];					\
char rcv_buffer_scattered2[5];					\
char rcv_buffer_scattered3[3];					\
struct iovec rcv_buffer_scattered_iovec[3] =			\
  {{ &rcv_buffer_scattered1, sizeof(rcv_buffer_scattered1)},	\
   { &rcv_buffer_scattered2, sizeof(rcv_buffer_scattered2)},	\
   { &rcv_buffer_scattered3, sizeof(rcv_buffer_scattered3)}}

int socket_fd[2];

/* In this test, we always send to the same socket and receive through
   the other. */
#define send_socket socket_fd[0]
#define rcv_socket socket_fd[1]

/* Clears rcv_buffers and writes data to send_socket. */
void init_reception(void)
{
  bzero( rcv_buffer, SIZEOF_RCV_BUFFER);
  write( send_socket, sent_msg, SIZEOF_SENT_MSG);
}

void init_sockets(void)
{
  /* Creates a pair of local sockets. */
  if( socketpair(AF_LOCAL,SOCK_SEQPACKET,0,socket_fd) != 0)
    {
      fprintf( stderr, "Could not create a pair of sockets\n");
      exit( EXIT_FAILURE);
    }
  //@ assert \initialized(&socket_fd[0..1]);
}

void test_read(void)
{
  init_reception();
  read( rcv_socket, rcv_buffer, SIZEOF_RCV_BUFFER);
  printf("Hello %s\n", rcv_buffer);
}

void test_readv(void)
{
  DECLARE_SCATTERED_RECEIVE_BUFFER;
  init_reception();
  readv( rcv_socket, rcv_buffer_scattered_iovec, 3);


  rcv_buffer_scattered1[0] == 0; /* Reduce to bottom if rcv_buffer_scattered1 is not initialized.  */
  /* @assert \true; */ 

  printf( "Hello %.2s%.3s\n", rcv_buffer_scattered1, rcv_buffer_scattered2);
}


void test_recvmsg(void)
{
  DECLARE_SCATTERED_RECEIVE_BUFFER;
  init_reception();
  struct msghdr hdr;
  hdr.msg_name = NULL;
  hdr.msg_namelen = 0;
  hdr.msg_iov = rcv_buffer_scattered_iovec;
  hdr.msg_iovlen = 3;
  hdr.msg_control = NULL;
  hdr.msg_controllen = 0;
  recvmsg( rcv_socket, &hdr, 0);

  rcv_buffer_scattered1[0] == 0;  /* Reduce to bottom if rcv_buffer_scattered1 is not initialized.  */
  /* @assert \true; */

  printf( "Hello %.2s%.3s\n", rcv_buffer_scattered1, rcv_buffer_scattered2);
}

volatile int nondet;
int test_server_echo() {
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd == -1) return 1;
  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = 0x2222;
  if (bind(fd, (struct sockaddr*)&addr, sizeof(addr))) return 5;
  if (listen(fd, 10)) return 20;
  socklen_t addrlen = sizeof(addr);
  int client_fd = nondet ?
    accept(fd, (struct sockaddr*)&addr, &addrlen) :
    accept(fd, NULL, NULL);
  if (client_fd == -1) return 100;
  char buf[64];
  int r = read(client_fd, buf, 64);
  if (r == -1) return 200;
  if (write(client_fd, buf, r) < r) return 300;
  if (close(client_fd)) return 400;
  if (close(fd)) return 400;
  return 0;
}

int main(int argc, char **argv)
{
  init_sockets();
  test_read();
  test_readv();
  test_recvmsg();
  int r = test_server_echo();
  return 0;
}
