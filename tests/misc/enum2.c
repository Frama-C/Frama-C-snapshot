/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main sizeof_enum1
*/

#define BIT_DE_SIGNE_1 (0x98765432)
#define BIT_DE_SIGNE_0 (0x12345678)
typedef enum {
  E1_MOINS1 = -1,
  E1_SGN1 = BIT_DE_SIGNE_1,
  E1_SGN0 = BIT_DE_SIGNE_0
  } E1 ;
unsigned char enum1_sgn1_positif (void) {
  unsigned char res = E1_SGN1 > 0;
  printf ("enum1_sgn1_positif = %d\n", res);
  return res; /* WARN : ppc->0 ; gcc->1 */
}
unsigned char enum1_sgn1_inf_sgn0 (void) {
  unsigned char res = E1_SGN1 < E1_SGN0;
  printf ("enum1_sgn1_inf_sgn0 = %d\n", res);
  return res; /* WARN : ppc->1 ; gcc->0 */
}
int sizeof_enum1 (void) {
  int res = sizeof (E1);
  printf ("sizeof_enum1 = %d\n", res);
  return res; /* WARN : ppc->4 ; gcc->8 */
} 
