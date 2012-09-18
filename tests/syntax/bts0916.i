/* run.config
   STDOPT: +"-keep-comments"
*/
/* Use frama-c with option -keep-comments */

void main() {
  int port=10;
  
  while (port-->0) // ( port & 0x80 ) == 0 )
    {
      ; /* wait for pin1 - Compliant*/
      /* wait for pin2 */ ; /*  Not compliant/*, comment before ; */
      ;/* wait for pin3 - Not compliant, no white-space char after ; */
    }
}
