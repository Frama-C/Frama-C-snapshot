/* run.config*
   STDOPT: #"-absolute-valid-range 0x20-0x23"
*/

int G;

typedef short u16;

static int detect_video(void *video_base)
{
   volatile u16 *p = (u16 *)video_base;
//   Frama_C_show_each_F(p,p[0]);
   u16 saved1 = p[0];
   u16 saved2 = p[1];
   int video_found = 1;


   p[0] = 0xAA55;
   p[1] = 0x55AA;
   if ( (p[0] != 0xAA55) || (p[1] != 0x55AA) )
       video_found = 0;

   p[0] = 0x55AA;
   p[1] = 0xAA55;
   if ( (p[0] != 0x55AA) || (p[1] != 0xAA55) )
       video_found = 0;

   p[0] = saved1;
   p[1] = saved2;

   return video_found;
} 

int main(void) {
	void * ADDR=(void*)0x20;
	return(detect_video(ADDR));
}
