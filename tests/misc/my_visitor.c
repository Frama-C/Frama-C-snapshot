/* run.config
EXECNOW: make -s tests/misc/my_visitor_plugin/my_visitor.cmxs
CMD: @frama-c@ -load-module ./tests/misc/my_visitor_plugin/my_visitor
OPT: -main f
*/
int f() {
  int y = 0;
  y++;
  /*@ assert y == 1; */
  return 0;
}
