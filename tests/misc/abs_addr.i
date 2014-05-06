/* run.config
   GCC:
   OPT: -val -deps -out -input -absolute-valid-range 0x20000-0x3FFFF -journal-disable
*/

unsigned short AutoTest[1000]={0};

unsigned char TstRomUcmm(void)
{
 union {unsigned char byte[2];unsigned short word;} rom;
 unsigned short chkrom;
 unsigned short *ptrom;


 ptrom = (unsigned short *)0x020000;
 chkrom = 0;

 while(ptrom != (unsigned short *) 0x02FFFE)
 {
  rom.word = *ptrom;
  chkrom = chkrom + rom.byte[0] + rom.byte[1];
  ptrom++;
 }
 if(chkrom != *ptrom)
 {
  AutoTest[73] = (unsigned short)1;
 }


 ptrom = (unsigned short *)0x030000;
 chkrom = 0;
 while(ptrom != (unsigned short *) 0x03FFFE)
 {
  rom.word = *ptrom;
  chkrom = chkrom + rom.byte[0] + rom.byte[1];
  ptrom++;
 }
 if(chkrom != *ptrom)
 {
  AutoTest[73] = (unsigned short)1;
 }
 return(AutoTest[73]);
}

void main(void){
  TstRomUcmm();
}
