/*  run.config
   EXECNOW: make -s tests/slicing/switch.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module tests/slicing/switch.cmxs
   OPT: -check -deps -journal-disable
*/
int main (char choix) {
  int x = 0, y = 0, z = 0;
  switch (choix) {
    case 'a' : x = 1; break;
    case 'b' : x = 2; y = 1; break;
    case 'c' :
    case 'd' : y = 2; break;
  }
  z++;
  return x;
}
