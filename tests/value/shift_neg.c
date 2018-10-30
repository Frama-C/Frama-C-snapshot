/* run.config*
   STDOPT: #""
   STDOPT: #"-no-warn-left-shift-negative -warn-right-shift-negative"
*/

/* The first run emits alarms on left shifts on negative values;
   the second run emits alarms on right shifts on negative values. */

#include <__fc_builtin.h>

volatile int rand;

/* Tests left and right shift of negative integers. */
void main() {
  int x, r;
  // no alarm
  x = Frama_C_interval(24, 128);
  r = x >> 4;
  r = x << 4;
  // alarm and reduction of [x]
  x = Frama_C_interval(-8, 12);
  r = x << 2;
  Frama_C_show_each_left_shift(x, r);
  x = Frama_C_interval(-8, 12);
  r = x >> 2;
  Frama_C_show_each_right_shift(x, r);
  // invalid alarm
  if (rand) {
    int k = (-44) << 15;
    int l = (-44) << 3;
    Frama_C_show_each_neg_left_shift(k, l);
  }
  if (rand) {
    int i = (-44) >> 15;
    int j = (-44) >> 3;
    Frama_C_show_each_neg_right_shift(i, j);
  }
}
