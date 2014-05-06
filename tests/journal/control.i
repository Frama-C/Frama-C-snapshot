/* run.config
   COMMENT: do not compare generated journals since they depend on current time
   EXECNOW: BIN control_journal.ml BIN control_journal_bis.ml (./bin/toplevel.opt -journal-enable -val -deps -out -main f -journal-name tests/journal/result/control_journal.ml tests/journal/control.i && cp tests/journal/result/control_journal.ml tests/journal/result/control_journal_bis.ml) > /dev/null 2> /dev/null
  CMD: FRAMAC_LIB=lib/fc ./bin/toplevel.byte
  OPT: -load-script tests/journal/result/control_journal -journal-disable
  CMD: FRAMAC_LIB=lib/fc ./bin/toplevel.byte
  OPT: -load-script tests/journal/result/control_journal_bis -calldeps -journal-disable
  EXECNOW: BIN abstract_cpt_journal.ml FRAMAC_LIB=lib/fc ./bin/toplevel.byte -journal-enable -load-script tests/journal/abstract_cpt.ml -load-script tests/journal/use_cpt.ml -journal-name tests/journal/result/abstract_cpt_journal.ml > /dev/null 2> /dev/null
  CMD: FRAMAC_LIB=lib/fc ./bin/toplevel.byte
  OPT: -load-script tests/journal/result/abstract_cpt_journal.ml -load-script tests/journal/abstract_cpt.ml -load-script tests/journal/use_cpt.ml
  
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
