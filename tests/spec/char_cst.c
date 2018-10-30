/*@ requires c != '0';
  behavior quote: assumes c=='\'' ;
  behavior default: assumes c!='\'' && c!='a';
  behavior slash: assumes c=='\\' ;
  behavior other: assumes c!='\\' && c!='a';
  behavior hexa: assumes c!='\xAB';
  behavior oct: assumes c!='\123';
  behavior string: assumes ""!="\"" && ""=="" ;
  behavior esc: assumes c == ' ' || c == '\f' || c == '\n' ||
                        c == '\r' || c == '\t' || c == '\v';
*/
void f(char c) { }
