/* run.config
   STDOPT: +"-metrics-ast cil"
**/

void printf(const char *format);

int complexity5(int n){
  if (n > 0) {
    switch (n) {
    case 0 : case 1:
      printf("Zero or one\n");
      break;
    case 2:
      printf("Two\n");
      break;
    case 3: case 4:
      printf("Three or four\n");
      break;
    default: break;
    }}
  else printf("Negative\n");
  return(n);
}


int main() {
  return complexity5(1);
}
