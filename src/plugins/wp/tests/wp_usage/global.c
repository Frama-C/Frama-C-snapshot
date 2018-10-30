/* run.config
   OPT: -wp-model raw
   OPT: -wp-model var
   OPT: -wp-model ref
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
