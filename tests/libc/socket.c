/* run.config
   STDOPT: +"-val-builtin memset:Frama_C_memset"
   STDOPT: +"-val-builtin memset:Frama_C_memset -cpp-extra-args='-D__FRAMA_C_MACHDEP_X86_64'"
*/
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/uio.h>

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
const struct iovec rcv_buffer_scattered_iovec[3] =		\
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
  memset( rcv_buffer, 0, SIZEOF_RCV_BUFFER);
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


int main(int argc, char **argv)
{
  init_sockets();
  test_read();
  test_readv();
  test_recvmsg();

  return 0;
}


/*
Local Variables:
compile-command: "cd ../.. && ptests.byte -show -config gcc tests/libc/socket.c"
End:
*/
