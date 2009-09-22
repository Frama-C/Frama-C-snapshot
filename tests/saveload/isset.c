/* run.config
   EXECNOW: LOG isset_sav.res LOG isset_sav.err BIN isset.sav ./bin/toplevel.opt -quiet -val -save tests/saveload/result/isset.sav tests/saveload/isset.c > ./tests/saveload/result/isset_sav.res 2> ./tests/saveload/result/isset_sav.err
   OPT: -quiet -load ./tests/saveload/result/isset.sav
   OPT: -load ./tests/saveload/result/isset.sav
   OPT: -val -load ./tests/saveload/result/isset.sav
   OPT: -quiet -val -load ./tests/saveload/result/isset.sav
*/

int main() {
  int i, j;

  i = 10;
  while(i--);
  j = 5;

  return 0;
}
