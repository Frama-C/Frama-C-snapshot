/* run.config
  EXECNOW: BIN control_journal2.ml ./bin/toplevel.opt -journal-enable -val -deps -out -main f -journal-name tests/journal/result/control_journal2.ml tests/journal/control2.c > /dev/null 2> /dev/null
  EXECNOW: LOG control2_sav.res LOG control2_sav.err BIN control_journal_next2.ml FRAMAC_LIB=lib/fc ./bin/toplevel.byte -journal-enable -load-script tests/journal/result/control_journal2 -lib-entry -journal-name tests/journal/result/control_journal_next2.ml tests/journal/control2.c > ./tests/journal/result/control2_sav.res 2> ./tests/journal/result/control2_sav.err
  CMD: FRAMAC_LIB=lib/fc ./bin/toplevel.byte
  OPT: -load-script tests/journal/result/control_journal_next2
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
