#include "../apache.h"

void escape_absolute_uri (char *uri, int scheme)
{
  int cp;
  char *token[TOKEN_SZ];
  int c;

  cp = scheme;

  if (strncmp(uri, LDAP, LDAP_SZ) == 0) {
    c = 0;
    token[0] = uri;
    while (uri[cp] != EOS
           && c < TOKEN_SZ) {
      if (uri[cp] == '?') {
        ++c;
        /* BAD */
        token[c] = uri + cp + 1;
        uri[cp] = EOS;
      }
      ++cp;
    }
    return;
  }

  return;
}

int main ()
{
  char uri [URI_SZ];
  int scheme;

  uri [URI_SZ-1] = EOS;
  scheme = LDAP_SZ + 2;

  escape_absolute_uri (uri, scheme);

  return 0;
}
