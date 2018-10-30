/* run.config
   OPT:
   EXECNOW: LOG GNUmakefile LOG make_template.res LOG make_template.err bin/frama-c-script make-template @PTEST_DIR@/result < @PTEST_DIR@/make_template.input > @PTEST_DIR@/result/make_template.res 2> @PTEST_DIR@/result/make_template.err
   EXECNOW: LOG list_files.res LOG list_files.err bin/frama-c-script list-files @PTEST_DIR@/list_files.json > @PTEST_DIR@/result/list_files.res 2> @PTEST_DIR@/result/list_files.err
   EXECNOW: LOG flamegraph.html LOG flamegraph.res LOG flamegraph.err NOGUI=1 bin/frama-c-script flamegraph @PTEST_DIR@/flamegraph.txt @PTEST_DIR@/result > @PTEST_DIR@/result/flamegraph.res 2> @PTEST_DIR@/result/flamegraph.err && rm -f @PTEST_DIR@/result/flamegraph.svg
 */

void main() {

}
