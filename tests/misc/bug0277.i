/* run.config
   OPT: -typecheck
 */
typedef enum {
	DGI_ID_NB = 56
} T_DGI_ID;



const int T[DGI_ID_NB] = { 3 } ;



/*@

requires P : T[0]==3
 ;


*/
void main() ;
