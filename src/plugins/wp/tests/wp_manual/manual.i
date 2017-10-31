/* run.config
   DONTRUN:
*/
/* run.config_qualif
   OPT: -wp-msg-key no-time-info @PTEST_DIR@/working_dir/swap.c @PTEST_DIR@/working_dir/swap1.h
   OPT: -wp-msg-key no-time-info -wp-rte @PTEST_DIR@/working_dir/swap.c @PTEST_DIR@/working_dir/swap2.h
   OPT: -load-module report -kernel-verbose 0 -wp-msg-key no-time-info -wp-rte @PTEST_DIR@/working_dir/swap.c @PTEST_DIR@/working_dir/swap2.h -wp-verbose 0 -then -no-unicode -report  
*/
void look_at_working_dir(void);
