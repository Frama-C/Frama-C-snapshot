/* run.config
   EXECNOW: make -s ./tests/saveload/size.opt
   EXECNOW: LOG size_sav.res LOG size_sav.err BIN size.sav ./tests/saveload/size.opt -val -out -input -deps ./tests/saveload/size.c -save ./tests/saveload/result/size.sav > ./tests/saveload/result/size_sav.res 2> ./tests/saveload/result/size_sav.err
   EXECNOW: LOG size_sav.1.res LOG size_sav.1.err BIN size.1.sav ./bin/toplevel.opt -save ./tests/saveload/result/size.1.sav ./tests/saveload/size.c -val -out -input -deps > ./tests/saveload/result/size_sav.1.res 2> ./tests/saveload/result/size_sav.1.err
   OPT: -load ./tests/saveload/result/size.sav -val -out -input -deps -journal-disable
   CMD: ./tests/saveload/size.opt
   OPT: -load ./tests/saveload/result/size.1.sav -val -out -input -deps -journal-disable
   OPT: -load ./tests/saveload/result/size.1.sav -val -out -input -deps -journal-disable
*/

int main() {
  int i, j;

  i = 10;
  while(i--);
  j = 5;

  return 0;
}
