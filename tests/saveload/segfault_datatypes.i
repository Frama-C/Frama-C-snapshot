/* run.config
   EXECNOW: make -s ./tests/saveload/segfault_datatypes_A.opt
   EXECNOW: make -s ./tests/saveload/segfault_datatypes_B.opt
   EXECNOW: LOG segfault_datatypes_sav.res LOG segfault_datatypes_sav.err BIN segfault_datatypes.sav ./tests/saveload/segfault_datatypes_A.opt -val -out -input -deps ./tests/saveload/segfault_datatypes.i -save ./tests/saveload/result/segfault_datatypes.sav > ./tests/saveload/result/segfault_datatypes_sav.res 2> ./tests/saveload/result/segfault_datatypes_sav.err
   CMD: ./tests/saveload/segfault_datatypes_B.opt
   OPT: -load ./tests/saveload/result/segfault_datatypes.sav -val -out -input -deps -journal-disable
*/

int main() {
  int i, j;

  i = 10;
  while(i--);
  j = 5;

  return 0;
}
