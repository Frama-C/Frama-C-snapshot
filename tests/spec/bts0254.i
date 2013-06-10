/*@ behavior d: assumes \true; 
  assumes \false; 
  requires \true;
  ensures
  \true && x && \true && x && \true && x && \true && x && \true && x && \true;
*/
int f(int x) {
  return 0;
};

/*@
  requires \true; 
ensures \false; 
 assigns \nothing; */
int g(void) {
   return 0;
 };


/*@
  requires \true;
  terminates \false;
  decreases x;
  ensures \false; 
  assigns \nothing;
  behavior b1: assumes \true;
  behavior b2: assumes \false;
  disjoint behaviors b1, b2;
 */
int h(int x) {
   return 0;
 };

/*@ requires \true; */
int a(void) {
   return 0;
 };

 /*@ behavior d:
      ensures \true;
 */
 int bts(void) {
   return 0;
 };
