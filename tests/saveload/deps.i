/* run.config
   EXECNOW: make -s ./tests/saveload/deps_A.opt ./tests/saveload/deps_B.opt ./tests/saveload/deps_C.opt ./tests/saveload/deps_D.opt ./tests/saveload/deps_E.opt
   EXECNOW: LOG deps_sav.res LOG deps_sav.err BIN deps.sav ./tests/saveload/deps_A.opt -val -out -input -deps ./tests/saveload/deps.i -save ./tests/saveload/result/deps.sav > ./tests/saveload/result/deps_sav.res 2> ./tests/saveload/result/deps_sav.err
   CMD: ./tests/saveload/deps_A.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps
   CMD: ./tests/saveload/deps_B.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps
   CMD: ./tests/saveload/deps_C.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps
   CMD: ./tests/saveload/deps_D.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps
   CMD: ./tests/saveload/deps_E.opt
   OPT: -load ./tests/saveload/result/deps.sav -val -out -input -deps
*/

int main() {
  int i, j;

  i = 10;
  while(i--);
  j = 5;

  return 0;
}
