#include "../constants.h"

static NSS_STATUS
_nss_winbind_ipnodes_getbyname(char *name)
{
  char winsreq [FSTRING_LEN];

  /* BAD */
  r_strncpy(winsreq, name, strlen(name)) ;

  return 0;
}

int main ()
{
  char in [INSZ];
  in[INSZ-1] = EOS;

  _nss_winbind_ipnodes_getbyname(in);

  return 0;
}
