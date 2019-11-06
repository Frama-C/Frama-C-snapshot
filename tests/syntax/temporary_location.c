/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -print
*/

int f(void) {
    return 1;
}

int main(void) {
    if(f()){
        return 0;
    }
    else {
        int x = 0;
        int y = x++ + 1;
        return y + 3;
    }
}
