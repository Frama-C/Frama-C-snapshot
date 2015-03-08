/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-rte
*/

void f(unsigned char *t) {
//@ assert t[0] < 256;
        t[1]=3;
}
