#include "../gd.h"

static int
gdTcl_UtfToUniChar (char *str, Tcl_UniChar * chPtr)
{
  return 1;
}


/* Greatly, GREATLY simplified. There's a bunch of cruft that doesn't
 * have to do with the manipulation of "string". */
void gdImageStringFTEx (char *string) {
  int next;
  int encoding;
  int i;
  int ch;
  int len;

  encoding = nondet_int();
  if (encoding > 2 || encoding < 0)
    return;

  next = 0;
  /* Unsafe read -- next can be out of bounds. */
  /* BAD */
  for (i=0; string[next] != EOS; i++)
    {
      /* grabbing a character and storing it in an int
       *
       * this'll fill the low-order byte, and keep more space free for
       * extra bytes for Unicode encoding, etc.
       */
      ch = string[next];

      /* carriage returns */
      if (ch == '\r')
	{
	  next++;
	  continue;
	}
      /* newlines */
      if (ch == '\n')
	{
	  next++;
	  continue;
	}


      switch (encoding)
        {
        case gdFTEX_Unicode:
          {
            len = gdTcl_UtfToUniChar (string + next, &ch);
            next += len;
          }
          break;
        case gdFTEX_Shift_JIS:
          {
            unsigned char c;
            c = (unsigned char) string[next];
            if (0xA1 <= c && c <= 0xFE)
              {
                next++;
              }
            /* We're incrementing next twice, which could make us skip
             * the terminating EOS character. The read of "string"
             * could then be out of bounds. */
            next++;
          }
          break;
        case gdFTEX_Big5:
          {
            ch = (string[next]) & 0xFF;	/* don't extend sign */
            next++;
            if (ch >= 161	/* first code of JIS-8 pair */
                && string[next] != EOS)
              {
                ch = (ch * 256) + ((string[next]) & 255);
                next++;
              }
          }
          break;
        }
    }
}

int main ()
{
  char in [INSZ];
  in [INSZ-1] = EOS;

  gdImageStringFTEx (in);

  return 0;
}

