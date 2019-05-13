/*run.config
  STDOPT:
*/
#include <pwd.h>
#include "__fc_string_axiomatic.h"

extern uid_t uid;

int main() {
  struct passwd *pw = getpwuid(uid);
  if (pw) {
    //Note: the assertions below are currently imprecise
    //@ assert valid_read_string(pw->pw_name);
    //@ assert valid_read_string(pw->pw_passwd);
    //@ assert valid_read_string(pw->pw_dir);
    //@ assert valid_read_string(pw->pw_shell);
  }
  pw = getpwnam("root");
  if (pw) {
    //Note: the assertions below are currently imprecise
    //@ assert valid_read_string(pw->pw_name);
    //@ assert valid_read_string(pw->pw_passwd);
    //@ assert valid_read_string(pw->pw_dir);
    //@ assert valid_read_string(pw->pw_shell);
  }
}
