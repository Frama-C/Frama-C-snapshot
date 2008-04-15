/* run.config
   EXECNOW: make -s ./tests/saveload/deps_A.opt
   EXECNOW: make -s ./tests/saveload/deps_B.opt
   EXECNOW: make -s ./tests/saveload/deps_C.opt
   EXECNOW: make -s ./tests/saveload/deps_D.opt
   EXECNOW: LOG deps_sav.res LOG deps_sav.err BIN deps.sav ./tests/saveload/deps_A.opt -val -out -input -deps ./tests/saveload/deps.c -save ./tests/saveload/result/deps.sav -journal-disable > ./tests/saveload/result/deps_sav.res 2> ./tests/saveload/result/deps_sav.err
   CMD: ./tests/saveload/deps_A.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps -journal-disable
   CMD: ./tests/saveload/deps_B.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps -journal-disable
   CMD: ./tests/saveload/deps_C.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps -journal-disable
   CMD: ./tests/saveload/deps_D.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps -journal-disable
*/

int main() {
  int i, j;

  i = 10;
  while(i--);
  j = 5;

  return 0;
}
