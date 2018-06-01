/*@ predicate foo (integer x) =
  x <= 0 && (x <= 1 && x<= 3) ==>
   x<=4 || ((x<=5 && x<=6) || x<=7) && x<=8;
 */

/*@ predicate bar(integer x, integer y, integer z, integer t) =
  x == 0 || (y == 0 || (z == 0 || t == 0));
*/

/*@ predicate mixed(integer x, integer y, integer z, integer t) =
  x == 0 || ((y == 0 || z == 0) || t == 0);
*/

/*@ logic real pi = \pi;
*/

/*@ logic real pi_div_2 = π/2;
*/

/*@ logic real e = \e;
*/

/*@ logic real exp(real n) = \pow(\e, n);
*/
