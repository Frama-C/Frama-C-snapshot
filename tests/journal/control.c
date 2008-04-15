/* run.config
   EXECNOW: BIN control_journal.ml ./bin/toplevel.opt -memory-footprint 1 -val -deps -out -main f -journal-name tests/journal/result/control_journal tests/journal/control.c > /dev/null 2> /dev/null
  EXECNOW: make -s tests/journal/result/control_journal.cmo
  CMD: ./bin/toplevel.byte
  OPT: -load-journal tests/journal/result/control_journal.cmo -journal-disable
  CMD: ./bin/toplevel.byte
  OPT: -load-journal tests/journal/result/control_journal.cmo -calldeps -journal-disable
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
