/* run.config
   OPT: tests/misc/merge_bts0948_1.i tests/misc/merge_bts0948_2.i -check -print
*/

/*@ requires \valid((char*)dest);
*/
extern void *memcpy(void * dest);

void* memcpy(void* region1) { return region1; }
