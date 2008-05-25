/* see bug #137 */
/*@ requires c != '0';
  behavior default: assumes c!='\'';
  behavior hexa: assumes c != '\xAB';
  behavior oct: assumes c!= '\123';
*/
void f(char c) { }
