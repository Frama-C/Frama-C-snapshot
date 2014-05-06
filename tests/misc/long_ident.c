/* run.config
   OPT: -load-module lib/plugins/Obfuscator -obfuscate -journal-disable
*/

/*@ ensures \valid(q);  // <-- obfuscation error [bts#404]
 */
int f(int *q) ;

#define LV X_9999999999999999999999999999999999999999999999999999
int LV;
enum { OK = 1,
       NOT_OK = 0 } e ;

/*@ ensures \valid(p);
 */
void main (int LV, int * p) {
int LV = 0;
 e = OK ; // <-- obfuscation error [bts#403]
 f(p);
}
