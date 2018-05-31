/* run.config
   OPT: -wp-model raw -wp-warn-separation
   OPT: -wp-model var -wp-warn-separation
   OPT: -wp-model ref -wp-warn-separation
 */

/* run.config_qualif
   DONTRUN:
*/

int GLOBAL ;

/*@
    requires \valid(a);
*/
void foo(int* a) {
    /*@ assert no_address_taken: \separated(a, &GLOBAL); */
    *a = 42;
}

int main() { foo(&GLOBAL); 
}
