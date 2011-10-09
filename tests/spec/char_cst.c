/*@ requires c != '0';
  behavior quote: assumes c=='\'' ;
  behavior default: assumes c!='\'' && c!='a';
  behavior slash: assumes c=='\\' ;
  behavior other: assumes c!='\\' && c!='a';
  behavior hexa: assumes c!='\xAB';
  behavior oct: assumes c!='\123';
  behavior string: assumes ""!="\"" && ""=="" ;
*/
void f(char c) { }
