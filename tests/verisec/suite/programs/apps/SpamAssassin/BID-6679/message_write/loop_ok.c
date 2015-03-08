#include "../constants.h"

void message_write (char *msg, int len)
{
  int i;
  int j;
  char buffer[BUFSZ];

  int limit = BUFSZ - 4;

  for (i = 0; i < len; ) {
    for (j = 0; i < len && j < limit; ){
      if (i + 1 < len 
          && msg[i] == '\n' 
          && msg[i+1]== '.') {
        buffer[j] = msg[i];
        j++;
        i++;
        buffer[j] = msg[i];
        j++;
        i++;
        /* OK */
        buffer[j] = '.';
        j++;
      } else {
        buffer[j] = msg[i];
        j++;
        i++;
      }
    }
  }
}

int main ()
{
  char msg [INSZ] = "message";

  message_write (msg, INSZ);
  
  return 0;
}

