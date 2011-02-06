/* run.config
  OPT:  -journal-disable -wp -wp-model Hoare -wp-proof none -wp-print
*/
  
/* run.config_phoare
  OPT:  -journal-disable -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

// Tests on arithmertic problem : only needs Hoare.
// See [wp_bits.c] for examples on bit representation.


/*@ behavior KO : ensures \result == 10;
    behavior ok : ensures \result == 0;
*/
char implicit_char_cast (void) {
  unsigned char i = 0;
  signed char x = -1;
  // in this example, both [char] are first promoted to [int],
  // so the comparition gives the intuitive result (see [implicit_cast] below)
  return (x < i) ? 0 : 10;
}

/*@ behavior ok : ensures \result == 10;
    behavior KO : ensures \result == 0;
*/
int implicit_cast (void) {
  unsigned int i = 0;
  signed int x = -1;
  // in this example, both variables are converted to [unsigned int],
  // so the result of the comparison looks strange...
  return (x < i) ? 0 : 10;
}

/*@ behavior KO : ensures \result < 0;
    behavior ok : ensures \result >= 0;
*/
unsigned char cast_sgn_usgn (void) {
  char x = -1;
  return x;
}

// This one is ok.
unsigned char uchar_range (unsigned char i) {
  //@ assert i >= 0;
  //@ assert i <= 255;
  return i;
}


