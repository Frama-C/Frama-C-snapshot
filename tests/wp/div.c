
/*@
  behavior pos_pos : 
   assumes (a == 3) && (b == 2);
   ensures \result == 1; 

  behavior neg_neg : 
   assumes (a == -3) && (b == -2);
   ensures \result == 1; 

  behavior neg_pos : 
   assumes (a == -3) && (b == 2) ; 
   ensures \result == -1; 

  behavior pos_neg : 
   assumes (a == 3) && (b == -2);
   ensures \result == -1; 
*/


int f (int a,int b) {return (a/b);}


