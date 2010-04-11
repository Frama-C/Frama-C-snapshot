/* run.config
EXECNOW: make -s tests/misc/my_visitor_plugin/my_visitor.opt
EXECNOW: make -s tests/misc/dashtbl_plugin/dashtbl.opt
CMD: ./tests/misc/my_visitor_plugin/my_visitor.opt
OPT: -main f
CMD: ./tests/misc/dashtbl_plugin/dashtbl.opt
OPT: -main f
*/
int f() {
  int y = 0;
  y++;
  /*@ assert y == 1; */
  return 0;
}
