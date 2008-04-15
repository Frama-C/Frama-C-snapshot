/* run.config
   GCC:
   OPT: -security-slicing -slevel 8 -calldeps -pp-annot -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -security-lattice strong -calldeps -pp-annot  -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -calldeps -main main2 -pp-annot -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -security-lattice strong -calldeps -main main2 -pp-annot  -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -calldeps -main main3 -pp-annot -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -security-lattice strong -calldeps -main main3 -pp-annot -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -calldeps -main main4 -pp-annot -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -security-lattice strong -calldeps -main main4 -pp-annot -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -calldeps -main main5 -pp-annot -slice-print -journal-disable
   OPT: -security-slicing -slevel 8 -security-lattice strong -calldeps -main main5 -pp-annot -slice-print -journal-disable
   */

//#define GCC
#ifdef GCC
  #include <stdlib.h>
  #include <stdio.h>
#else
  #define FRAMA_C_MALLOC_INDIVIDUAL
  #include <share/malloc.c>
#endif

#define CRYPT_PAD 1
#define DATA_LEN_MOINS_UN 7

#define IP_FIELD 4
typedef int ip_address[IP_FIELD];

typedef struct {
  ip_address src;
  ip_address dst;
  char *data;
} msg;

char *my_strcpy (char *dst, const char *src) {
   char *p = dst;
   while((*dst++ = *src++));
   return p;
}

unsigned int my_strlen(const char *s)
{
  unsigned int l = 0;
  while(s[l] != 0) l++;
  return l;
}

/*@ ghost int channel; */

/*@ requires security_status(addr) == public;
  assigns channel \from addr[0..3]; */
void send_addr(const ip_address addr, const char *txt)
#ifdef GCC
{
  int i;
  printf("%s", txt);
  for(i=0; i<IP_FIELD-1; i++) printf("%d.", addr[i]);
  printf("%d\n", addr[IP_FIELD-1]);
}
#else
  ;
#endif

/*@ requires security_status(data) == public;
  assigns channel \from data[0.. DATA_LEN_MOINS_UN]; */
void send_data(const char *data, const char *txt)
#ifdef GCC
{
  printf("%s%s", txt, data);
}
#else
  ;
#endif

void send_msg(const msg *msg) {
  send_addr(msg->src,  "source = ");
  send_addr(msg->dst,  "dest   = ");
  send_data(msg->data, "msg    = ");
#ifdef GCC
  printf("\n");
#endif
}

/*@ assigns channel \from msg,
  msg->src[0..3], msg->dst[0..3], msg->data[0.. DATA_LEN_MOINS_UN];
*/
void send_msg_safely(const msg *msg)
#ifdef GCC
{
  printf("send safely\n");
}
#else
;
#endif



/*@ ensures security_status(data) == public;
  assigns data[0.. DATA_LEN_MOINS_UN] \from data[0.. DATA_LEN_MOINS_UN]; */
void crypt(char* data)
#ifdef GCC
{
  unsigned int i = 0;
  char *d = data;
  while(d[i]!='\0') { d[i] += CRYPT_PAD; i++; }
}
#else
;
#endif

/*@ ensures security_status(data) == private;
  assigns data[0.. DATA_LEN_MOINS_UN] \from data[0.. DATA_LEN_MOINS_UN]; */
void uncrypt(char* data)
#ifdef GCC
{
  unsigned int i = 0;
  char *d = data;
  while(d[i]!='\0') { d[i] -= CRYPT_PAD; i++; }
}
#else
;
#endif

/*@ ensures security_status(ip) == public; */
void host_address(ip_address ip) {
  ip[0] = 192; ip[1] = 100; ip[2] = 200; ip[3] = 101;
}

void create_msg(msg *msg, const ip_address dst, const char *data) {
  unsigned int i;
  host_address(msg->src);
  for(i=0; i<IP_FIELD; i++) msg->dst[i] = dst[i];
  msg->data = malloc(my_strlen(data)+1);
  my_strcpy(msg->data, data);
}

int main() {
  msg msg1, msg2;
  ip_address /*@ public */ dst = { 192, 100, 200, 102 };
  ip_address /*@ public */ dst2 = { 192, 100, 200, 102 };

  create_msg(&msg1, dst, "Virgile");
  send_msg_safely(&msg1);

  create_msg(&msg2, dst2, "Benjamin");
  send_msg_safely(&msg2);

  create_msg(&msg1, dst, "Julien");
  send_msg(&msg1); // faille sur msg1->data

  crypt(msg1.data);
  send_msg(&msg1);

  uncrypt(msg1.data);
  send_msg(&msg1); // faille sur msg1->data
  return 0;
}

/* ************************************************************************** */

int main2() {
  msg msg1, msg2;
  ip_address /*@ public */ dst = { 192, 100, 200, 102 };
  ip_address /*@ public */ dst2 = { 192, 100, 200, 102 };

  create_msg(&msg1, dst, "Virgile");
  send_msg_safely(&msg1);

  create_msg(&msg2, dst2, "Benjamin");
  send_msg_safely(&msg2);

  create_msg(&msg1, dst, "Julien");
  send_msg(&msg1); // faille sur msg1->data

  crypt(msg1.data);
  uncrypt(msg1.data);

  return 0;
}

/* ************************************************************************** */

int main3() {
  msg msg1, msg2;
  ip_address /*@ public */ dst = { 192, 100, 200, 102 };
  ip_address /*@ public */ dst2 = { 192, 100, 200, 102 };

  create_msg(&msg1, dst, "Virgile");
  send_msg_safely(&msg1);

  create_msg(&msg2, dst2, "Benjamin");
  send_msg_safely(&msg2);

  create_msg(&msg1, dst, "Julien");
  crypt(msg1.data);
  send_msg(&msg1);

  uncrypt(msg1.data);
  return 0;
}

/* ************************************************************************** */

int main4() {
  msg msg1, msg2;
  ip_address /*@ public */ dst = { 192, 100, 200, 102 };
  ip_address /*@ public */ dst2 = { 192, 100, 200, 102 };

  create_msg(&msg1, dst, "Virgile");
  send_msg_safely(&msg1);

  create_msg(&msg2, dst2, "Benjamin");
  send_msg_safely(&msg2);

  create_msg(&msg1, dst, "Julien");
  crypt(msg1.data);
  uncrypt(msg1.data);

  send_msg(&msg1); // faille sur msg1->data
  return 0;
}

/* ************************************************************************** */

/*@ assigns channel \from addr[0..3]; */
void send_addr_safely(const ip_address addr, const char *txt)
#ifdef GCC
{
  int i;
  printf("%s", txt);
  for(i=0; i<IP_FIELD-1; i++) printf("%d.", addr[i]);
  printf("%d\n", addr[IP_FIELD-1]);
}
#else
  ;
#endif

void send_msg5(const msg *msg) {
  send_addr_safely(msg->src,  "source = ");
  send_addr_safely(msg->dst,  "dest   = ");
  send_data(msg->data, "msg    = ");
#ifdef GCC
  printf("\n");
#endif
}

int main5() {
  msg msg1, msg2;
  ip_address /*@ public */ dst = { 192, 100, 200, 102 };
  ip_address /*@ public */ dst2 = { 192, 100, 200, 102 };

  create_msg(&msg1, dst, "Virgile");
  send_msg_safely(&msg1);

  create_msg(&msg2, dst2, "Benjamin");
  send_msg_safely(&msg2);

  create_msg(&msg1, dst, "Julien");
  send_msg5(&msg1); // faille sur msg1->data

  crypt(msg1.data);
  uncrypt(msg1.data);

  return 0;
}
