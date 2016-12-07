/* run.config_qualif
   DONTRUN:
*/

int x;

/*@
 requires x == 1;
 ensures x == 1;
*/
void bad (int e) {
 switch (e) {
 case 0: break;
 case 1: x = 2; break;
 }
}
