/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="-zbit"
*/

/*@ ensures \result == (int) (a & b & c); 
  @ ensures band0: 3!=(\result & 0xF0);
  @ behavior bit0:
  @   assumes bit0: 1==(a & 1) && 1==(b & 1) && 1==(c & 1);
  @   ensures band1: 1==(\result & 1);
  @   ensures band2: 0!=(\result & 1);
  @ behavior bit1:
  @   assumes bit1: 0==(b & 2);
  @   ensures band3: 0==(\result & 2);
  @ behavior bit2:
  @   assumes bit2: 0!=(c & 4);
  @   ensures band4: (\result & 4) == (a & b & 4);
  @ behavior bit3:
  @   assumes bit3: 2!=(a & 2) && 0==(b & c & 2) && 1 != (a & b & 1);
  @   ensures band5: (\result & 2) == (a & b & 1);
  @ behavior bit4: 
  @   assumes bit4: a==-1 && b==~0 && c==-1; 
  @   ensures band6: \result==-1;
  @ behavior bit5:
  @   ensures band7: zbit: (0x55==(0xFFF & a)) ==> (0x5555!=(0xFFFF & a));
 */
int band(int a,int b,int c) { return a & b & c; }

/*@ ensures \result == (int) (a | b | c); 
  @ ensures bor0: 3!=(\result | 0xF0);
  @ behavior bit1:
  @   assumes bit1: 2==(a & 2);
  @   ensures bor1: 2==(\result & 2);
  @ behavior bit2:
  @   assumes bit2: 0==(a & 4) && 0==((b | c) & 4);
  @   ensures bor2: 0==(\result & 4);
  @ behavior bit3: 
  @   assumes bit3: a==0 && b == 0 && c==0; 
  @   ensures bor3: \result==0;
 */
int bor(int a,int b, int c) { return a | b | c ; }

/*@ ensures \result == (int) (a ^ b);
  @ behavior bit1:
  @   assumes a == -1 && 0xFF==(0xF0^b);
  @   ensures \result != ~0xF;
  @ behavior bit2:
  @   assumes a == b;
  @   ensures \result == 0;
  @ behavior bit3:
  @   assumes a == ~b;
  @   ensures zbit: \result == -1;
 */ 
int bxor(int a,int b) { return a ^ b ; }

//@ ensures \result == (int) (~a) ;
int bnot(int a) { return ~a ; }

/*@ ensures \result == (int) (a << n) ;
  @  behavior shift1:
  @    assumes n == 3;
  @    ensures lsl1: ((a & 1) != 0) == (0 != (\result & 8));
  @    ensures lsl2: 1 != (\result & 1);
  @  behavior shift2:
  @    assumes a == 2;
  @    ensures lsl3: 0 != ( (a<<(unsigned)(n) ) & ((1 << (1+(unsigned)(n)) ))); 
*/
int lshift(int a,int n) { return a << n ; }

/*@ ensures \result == (int) (a >> n) ;
  @  behavior shift1:
  @    assumes n == 3;
  @    ensures lsr1: ((a & 8) != 0) == (0 != (\result & 1));
*/
int rshift(int a,int n) { return a >> n ; }

