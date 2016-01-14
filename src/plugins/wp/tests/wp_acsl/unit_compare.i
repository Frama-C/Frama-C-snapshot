/* run.config_qualif
  DONTRUN: only syntactic check.
*/


/*@ 
  requires LE_X_Y : x <= y ; 
  requires LE_X1_Y : x+1 <= y ;
  requires LE_X_Y1 : x <= y+1 ;
  requires LE_X1_Y1 : x+1 <= y+1 ;
  requires LE_Xm1_Y : x-1 <= y ;
  requires LE_X_Ym1 : x <= y-1 ;
  requires LE_Xm1_Ym1 : x-1 <= y-1 ;
  requires LE_X1_Ym1 : x+1 <= y-1 ;
  requires LE_Xm1_Y1 : x-1 <= y+1 ;
  requires LT_X_Y : x < y ; 
  requires LT_X1_Y : x+1 < y ;
  requires LT_X_Y1 : x < y+1 ;
  requires LT_X1_Y1 : x+1 < y+1 ;
  requires LT_Xm1_Y : x-1 < y ;
  requires LT_X_Ym1 : x < y-1 ;
  requires LT_Xm1_Ym1 : x-1 < y-1 ;
  requires LT_X1_Ym1 : x+1 < y-1 ;
  requires LT_Xm1_Y1 : x-1 < y+1 ;
*/

void main(int x,int y) {}
