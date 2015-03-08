#include "../apache.h"

void escape_absolute_uri (char *uri, int scheme)
{
  char *cp;
  char *token[TOKEN_SZ];
  int c;

  if (scheme == 0
      || strlen(uri) < scheme) {
    return;
  }

  cp = uri + scheme;

  if (cp[-1] == '/') {
    while (*cp != EOS && *cp != '/') {
      ++cp;
    }

    if (*cp == EOS || *(cp+1) == EOS) return;
    ++cp;

    scheme = cp - uri;

    if (strncmp(uri, LDAP, LDAP_SZ) == 0) {
      c = 0;
      token[0] = uri;
      while (*cp != EOS
             && c < TOKEN_SZ - 1) {
        if (*cp == '?') {
          ++c;
          /* OK */
          token[c] = cp + 1;
          *cp = EOS;
        }
        ++cp;
      }
      return;
    }
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
