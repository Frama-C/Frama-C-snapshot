/* run.config
   EXECNOW: make -s ./tests/saveload/segfault_datatypes_A.cmxs ./tests/saveload/segfault_datatypes_B.cmxs
   EXECNOW: LOG segfault_datatypes_sav.res LOG segfault_datatypes_sav.err BIN segfault_datatypes.sav @frama-c@ -load-module ./tests/saveload/segfault_datatypes_A -eva -out -input -deps ./tests/saveload/segfault_datatypes.i -eva-show-progress -save ./tests/saveload/result/segfault_datatypes.sav > ./tests/saveload/result/segfault_datatypes_sav.res 2> ./tests/saveload/result/segfault_datatypes_sav.err
   CMD: @frama-c@ -load-module ./tests/saveload/segfault_datatypes_B
   STDOPT: +"-load ./tests/saveload/result/segfault_datatypes.sav -eva -out -input -deps -journal-disable"
*/


int main() {
  int i, j;

  i = 10;
  while(i--);
  j = 5;

  return 0;
}
