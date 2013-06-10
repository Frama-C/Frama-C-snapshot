/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
*/

int main() {

  int i=0;  
  int x=0,y=0,z=0;
  unsigned int ux=0,uy=0,uz=0;
  long lx=0,ly=0,lz=0;

  z = 5u << 30;
  uz = 5 << 30;
  z = 5 << 30;

  z = -3 << 2;

  z = 5 << 30;
  lz = 5 << 30;
  lz = 5 << 60;

  z = 5 << 29;

  z = 5 << 28;

  z = 5 << 3;

  z = 5 << 1;

  for(i = 0 ; i < 10 ; ++i) {
    z = 1 << i ;
    z = i << 1 ;
    z = i << i ;
  }

  z = 3 >> -2;
  z = 3 >> 5;
  z = 3 >> 32;
  z= 3 >> 31;

  z = -5 >> 1;
 
  z = x >> y;
  uz = ((unsigned int) x) >> y;

  z = -2 >> 1;
  uz = ((unsigned int) -2) >> 1;

  z = 0 << 10;
  z = 0 >> 10;
  return 0;
}
