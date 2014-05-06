/* run.config
   OPT: -val -deps -out -input -absolute-valid-range 0-0xFF -journal-disable
*/

int* T = (int*)0;
int TAB[10];

void main() {
  int i;
  i = &TAB[*T];
}
