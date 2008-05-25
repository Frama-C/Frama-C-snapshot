/* run.config
   GCC:
   OPT: -security-analysis -ulevel 8
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

/*@ requires security_status(addr) == public(); */
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

/*@ requires security_status(data) == public(); */
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

/*@ ensures security_status(data) == public(); */
void crypt(char* data) {
  unsigned int i = 0;
  char *d = data;
  while(d[i]!='\0') { d[i] += CRYPT_PAD; i++; }
}

/*@ ensures security_status(data) == private(); */
void uncrypt(char* data) {
  unsigned int i = 0;
  char *d = data;
  while(d[i]!='\0') { d[i] -= CRYPT_PAD; i++; }
}

/*@ ensures security_status(ip) == public(); */
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
  msg msg1;
  ip_address /*@ public */ dst = { 192, 100, 200, 102 };

  create_msg(&msg1, dst, "Julien");
  send_msg(&msg1); // faille sur msg1->data

  crypt(msg1.data);
  send_msg(&msg1);

  uncrypt(msg1.data);
  send_msg(&msg1); // faille sur msg1->data
  return 0;
}
