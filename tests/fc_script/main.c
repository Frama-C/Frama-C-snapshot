/* run.config
   OPT:
   EXECNOW: LOG GNUmakefile LOG make_template.res LOG make_template.err PTESTS_TESTING= bin/frama-c-script make-template @PTEST_DIR@/result < @PTEST_DIR@/make_template.input > @PTEST_DIR@/result/make_template.res 2> @PTEST_DIR@/result/make_template.err
   EXECNOW: LOG list_files.res LOG list_files.err bin/frama-c-script list-files @PTEST_DIR@/list_files.json > @PTEST_DIR@/result/list_files.res 2> @PTEST_DIR@/result/list_files.err
   EXECNOW: LOG flamegraph.html LOG flamegraph.res LOG flamegraph.err NOGUI=1 bin/frama-c-script flamegraph @PTEST_DIR@/flamegraph.txt @PTEST_DIR@/result > @PTEST_DIR@/result/flamegraph.res 2> @PTEST_DIR@/result/flamegraph.err && rm -f @PTEST_DIR@/result/flamegraph.svg
   EXECNOW: LOG find_fun1.res LOG find_fun1.err bin/frama-c-script find-fun main2 @PTEST_DIR@ > @PTEST_DIR@/result/find_fun1.res 2> @PTEST_DIR@/result/find_fun1.err
   EXECNOW: LOG find_fun2.res LOG find_fun2.err bin/frama-c-script find-fun main3 @PTEST_DIR@ > @PTEST_DIR@/result/find_fun2.res 2> @PTEST_DIR@/result/find_fun2.err
   EXECNOW: LOG find_fun3.res LOG find_fun3.err bin/frama-c-script find-fun false_positive @PTEST_DIR@ > @PTEST_DIR@/result/find_fun3.res 2> @PTEST_DIR@/result/find_fun3.err
 */

void main() {

}
