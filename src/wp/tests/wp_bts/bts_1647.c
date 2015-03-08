/* run.config_qualif
   DONTRUN:
*/

#define NULL ((void *) 0L)

 /*@
    ensures e1: \result == \null;
    ensures e2: \result == 0;
    ensures e3: \result == (int *) \null;
    ensures e4: \result == (int *) 0;
    ensures e5: \result == (int *)((void *)0);
*/
int * f (void) {
  return NULL;
}
