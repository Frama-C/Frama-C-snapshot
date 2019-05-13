/* run.config*
   STDOPT: #"-eva-subdivide-non-linear 10"
*/

/* This file gathers some examples where a product of states may have no
   concretization (if the domains have inferred incompatible properties)
   without being bottom (if the inter-reduction between domains are insufficient
   to prove the incompatibility). The bottom could come from a bottom value,
   or from inconsistent statuses emitted for the same alarm.
   In both cases, an evaluation can lead to bottom without any alarm. The
   analysis should not crash on such cases, but they should be reported to the
   user, as they could also reveal a bug in some domains. */

#include "__fc_builtin.h"

/* Exhibits incompatible states between the cvalue and the equality domains.
   Based on the absence of precise backward propagation for x*x. */
void main1 () {
  int x = Frama_C_interval(0, 10);
  int y = x * x;
  int z = -1;
  if (y < 9) {
    if (x > 2)
      z = x * x; /* Incompatible states, revealed by the evaluation of x*x. */
    else
      z = -2;
  }
  else
    z = -3;
}

/* Exhibits incompatible states between the cvalue and the equality domain.
   Relies on the absence of precise backward propagation for 2*i. */
int main2 () {
  int t[2];
  int i = Frama_C_interval(0, 1);
  t[0] = i;
  int x = t[(2*i)/2];
  int y = -1;
  if (i > 0)
    y = t[(2*i)/2]; /* Incompatible states, revealed by inconsistent statuses
                       on the alarm for index bounds. */
  return y;
}

/* Exhibits incompatible states between the cvalue and the symbolic location
   domains during a subdivided evaluation: some subdivisions are indeed
   unreachable. This tests deeply relies on the strategy chosen to subdivide. */
void main3() {
  int t[10] = {1, 2, 3, 4, 5, 0, 6, 7, 8, 9};
  int i = Frama_C_interval(0, 9);
  int x = i / t[i];
  int y = i / t[i]; /* Due to the previous line, i=5 is impossible; the alarm
                       will get inconsistent statuses during the subdivision,
                       and should no longer appear for the complete evaluation
                       with the symbolic locations domain. */
}

void main () {
  main1();
  main2();
  main3();
}
