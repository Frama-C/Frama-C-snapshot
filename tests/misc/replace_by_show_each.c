/* run.config
 OPT: -val -inout-callwise -inout -calldeps
*/

#define show_each_1 Frama_C_show_each_1
#define show_each_2 Frama_C_show_each_2

void show_each_1() {
}

void show_each_2() {
}


//@ assigns \nothing;
void Frama_C_show_each_2();

int x = 0;


int main(int j) {
  int i = 1;
  show_each_2();
  i = 2;
  show_each_1();
  return i = (j+1);
}
