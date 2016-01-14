/* run.config
   EXECNOW: make -s ./tests/saveload/deps_A.cmxs ./tests/saveload/deps_B.cmxs ./tests/saveload/deps_C.cmxs ./tests/saveload/deps_D.cmxs ./tests/saveload/deps_E.cmxs
   EXECNOW: LOG deps_sav.res LOG deps_sav.err BIN deps.sav @frama-c@ -load-module ./tests/saveload/deps_A.cmxs -val -out -input -deps ./tests/saveload/deps.i -save ./tests/saveload/result/deps.sav > ./tests/saveload/result/deps_sav.res 2> ./tests/saveload/result/deps_sav.err
   OPT: -load-module ./tests/saveload/deps_A -load ./tests/saveload/result/deps.sav -val -out -input -deps
   OPT: -load-module ./tests/saveload/deps_B -load ./tests/saveload/result/deps.sav  -out -input -deps
   OPT: -load-module ./tests/saveload/deps_C -load ./tests/saveload/result/deps.sav  -out -input -deps
   OPT: -load-module ./tests/saveload/deps_D -load ./tests/saveload/result/deps.sav  -out -input -deps
   OPT: -load-module ./tests/saveload/deps_E -load ./tests/saveload/result/deps.sav  -out -input -deps
*/

int main() {
  int i, j;

  i = 10;
  while(i--);
  j = 5;

  return 0;
}
