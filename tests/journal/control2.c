/* run.config
  EXECNOW: BIN control_journal2.ml ./bin/toplevel.opt -memory-footprint 1 -val -deps -out -main f -journal-name tests/journal/result/control_journal2 tests/journal/control2.c > /dev/null 2> /dev/null
  EXECNOW: make -s tests/journal/result/control_journal2.cmo
  EXECNOW: LOG control2_sav.res LOG control2_sav.err BIN control_journal-next2.ml ./bin/toplevel.byte -load-journal tests/journal/result/control_journal2.cmo -lib-entry -journal-name tests/journal/result/control_journal-next2 tests/journal/control2.c > ./tests/journal/result/control2_sav.res 2> ./tests/journal/result/control2_sav.err
  EXECNOW: make -s tests/journal/result/control_journal-next2.cmo
  CMD: ./bin/toplevel.byte
  OPT: -load-journal tests/journal/result/control_journal-next2.cmo
*/

int x,y,c,d;


void f() {
  int i;
  for(i=0; i<4 ; i++) {
    if (c) { if (d) {y++;} else {x++;}}
    else {};
    x=x+1;
    }
}
