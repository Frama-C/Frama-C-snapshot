/*@
 behavior normal:
	assumes r && !x;
	ensures \result == 0;
 behavior f:
	assumes !r || x;
	ensures \result == -1;
*/
int max(int *r, double x) { 
  if (!r || x) return -1; 
  return 0; 
} 
