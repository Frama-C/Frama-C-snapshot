/* run.config 
   OPT: -rte -rte-no-all -rte-mem -print -journal-disable
*/
struct ArrayStruct {
  int data[10];
} buff;

int main (int i) {
  return buff.data[i] ;
}
