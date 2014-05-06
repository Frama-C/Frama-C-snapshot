/* run.config
   OPT: -load-script @PTEST_DIR@/@PTEST_NAME@.ml
 */
int main(void) {
    int x = 0;
    int y = 0;
    /*@ loop invariant invmerger: chekofv_invariant_1_1: x==y; */
    while (y < 10) {
        x++;
        if (x!=9) y++;
    }
    return 0;
}
