#include "stdlib.h"
/*@ assigns \nothing;
  @ exits \exit_status == state;
  @ ensures \false;
*/
void inconditional_exit(int state) {
  exit (state);
}
