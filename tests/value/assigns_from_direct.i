/* run.config*
   STDOPT: #"-from-verify-assigns"
*/

void f_valid(int a, int *b, int c);
void f_invalid_direct(int a, int *b, int c);
void f_invalid_address(int a, int *b, int c);
void f_invalid_condition(int a, int *b, int c);
void f_invalid_all(int a, int *b, int c);

void main(void){
  int x = 3;
  int y;
  f_valid(x,&y,1);
  f_invalid_direct(x,&y,1);
  f_invalid_address(x,&y,1);
  f_invalid_condition(x,&y,1);
  f_invalid_all(x,&y,1);
}

/*@ assigns *b \from a, (indirect:c), (indirect:b); */
void f_valid(int a, int *b, int c){
  if(c) {
    *b = a;
  }
  else
    *b = 0;
}

/*@ assigns *b \from (indirect:c), (indirect:b); */
void f_invalid_direct(int a, int *b, int c){
  if(c) {
    *b = a;
  }
  else
    *b = 0;
}

/*@ assigns *b \from a, (indirect:c); */
void f_invalid_address(int a, int *b, int c){
  if(c) {
    *b = a;
  }
  else
    *b = 0;
}

/*@ assigns *b \from a, (indirect:b); */
void f_invalid_condition(int a, int *b, int c){
  if(c) {
    *b = a;
  }
  else
    *b = 0;
}

/*@ assigns *b \from \nothing; */
void f_invalid_all(int a, int *b, int c){
  if(c) {
    *b = a;
  }
  else
    *b = 0;
}
