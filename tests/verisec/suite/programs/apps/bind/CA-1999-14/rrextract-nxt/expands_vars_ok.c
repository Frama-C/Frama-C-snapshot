#include "../bind.h"

#define BYTES_TO_SKIP 10

static int
rrextract(u_char *msg, int msglen, u_char *rrp, u_char *dname, int namelen)
{
  u_char *eom, *cp, *cp1, *rdatap;
  u_int class, type, dlen;
  int n;
  int n1,n2;
  u_int32_t ttl;
  u_char data[MAXDATA*2];
  data [(MAXDATA*2)-1] = EOS;

  cp = rrp;
  eom = msg + msglen;

  if ((n = dn_expand(msg, eom, cp, (char *) dname, namelen)) < 0) {
    return (-1);
  }
  
  cp += n;
  BOUNDS_CHECK(cp, 2*INT16SZ + INT32SZ + INT16SZ);
  GETSHORT(type, cp);
  GETSHORT(class, cp);
  
  if (class > CLASS_MAX) {
    return (-1);
  }
  GETLONG(ttl, cp);

  if (ttl > MAXIMUM_TTL) {
    ttl = 0;
  }
  GETSHORT(dlen, cp);
  BOUNDS_CHECK(cp, dlen);

  rdatap = cp;

  if (nondet_int()) {
    return (-1);
  }


  /* Cut the switch.... */

  n = dn_expand(msg, eom, cp, (char *)data, sizeof data);
	   
  if (n < 0) {
    return (-1);
  }

  if (nondet_int()) {
    return (-1);
  }
  cp += n;

  n1 = strlen((char *)data) + 1;
  cp1 = data + n1 + 1;

  n2 = dlen - n;
  if (n2 > sizeof data - n1) {
    return -1;
  }
  /* OK */
  r_memcpy(cp1, cp, n2);

  return 0;
}

int main(){
  
  int msglen, ret;
  u_char *dp;
  u_char name [NAMELEN];
  u_char msg [MSGLEN+BYTES_TO_SKIP];

  name [NAMELEN-1] = EOS;
  msg [MSGLEN+BYTES_TO_SKIP-1] = EOS;

  msglen = MSGLEN;
  dp = msg;

  ret = rrextract(msg, msglen, dp, name, NAMELEN);
  
  return 0;
  
}
