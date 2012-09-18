/* run.config
<<<<<<< .working
EXECNOW: make -s tests/misc/my_visitor_plugin/my_visitor.opt
CMD: ./tests/misc/my_visitor_plugin/my_visitor.opt
OPT: -main f
=======


OPT: -load-script tests/misc/my_visitor_plugin/my_visitor.ml -main f

OPT: -load-script tests/misc/dashtbl_plugin/dashtbl_plugin.ml -main f
>>>>>>> .merge-right.r18651
*/
int f() {
  int y = 0;
  y++;
  /*@ assert y == 1; */
  return 0;
}
