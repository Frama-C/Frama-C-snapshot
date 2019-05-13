/* run.config*
   STDOPT: #"-absolute-valid-range 0-0xFF"
*/

int* T = (int*)0;
int TAB[10];

void main() {
  int i;
  i = (int) &TAB[*T];
}
