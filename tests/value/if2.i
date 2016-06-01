/* run.config*
   STDOPT: #"-print "
*/

typedef enum {
    E1 = 0,
    E2 = 0x0001,
    E3 = 0x0002,
    E4 = 0x0004,
    E5 = 0x0008,
    E6 = 0x0010,
    E7 = 0x0020
} T1_t;

static T1_t G1;
T1_t G2 = E1;

int G3 = 75, G4;

int R;

volatile v;

enum  Bool {
  FALSE = 0,
  TRUE = 2
};

void main(void)
{
  if (E6 == G1)
    { G2 = G1; }

  if (0 == G4)
    { G3 = G4; }

  if (v)
    if (E1) {
      R = 5;
    } else {
      R = 6;
    }

  if (v)
    if (!E1) {
      R = 6;
    } else {
      R = 5;
    }

  if (v)
    if (E5) {
      R = 7;
    } else {
      R = 5;
    }

  if (v)
    if (!E5) {
      R = 5;
    } else {
      R = 7;
    }

  enum Bool b;

  b = FALSE;
  if (!b)
    R += !b;

  b = TRUE;
  if (b)
    R += b;

  b = 1;
  if (b)
    R += b;
  
  return;
}
