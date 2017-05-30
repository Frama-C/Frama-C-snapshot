/* run.config*
  GCC:
  STDOPT: #"-cpp-frama-c-compliant -cpp-command \"gcc -C -E -I. %1 -o %2\""
*/

/* This test of enums doubles with a test of the % syntax in -cpp-command */



#define BIT_DE_SIGNE_1 (0x98765432)
#define BIT_DE_SIGNE_0 (0x12345678)
typedef enum {
  E1_MOINS1 = -1,
  E1_SGN1 = BIT_DE_SIGNE_1,
  E1_SGN0 = BIT_DE_SIGNE_0
  } E1 ;

E1 f(E1 x) { E1 y = x; return x; }

unsigned char enum1_sgn1_positif (void) {
  unsigned char res = (f((E1)E1_SGN1)) > 0;
  Frama_C_show_each_enum1_sgn1_positif(res);
  return res; /* WARN : ppc->0 ; gcc->1 */
}
unsigned char enum1_sgn1_inf_sgn0 (void) {
  unsigned char res = E1_SGN1 < E1_SGN0;
  Frama_C_show_each_enum1_sgn1_inf_sgn0(res);
  return res; /* WARN : ppc->1 ; gcc->0 */
}
unsigned char must_be_one, must_be_zero;
int main (void) {
  int res = sizeof (E1);
  must_be_zero = enum1_sgn1_inf_sgn0();
  must_be_one = enum1_sgn1_positif();
  Frama_C_show_each_sizeof_enum1(res);
  return res; 
}
