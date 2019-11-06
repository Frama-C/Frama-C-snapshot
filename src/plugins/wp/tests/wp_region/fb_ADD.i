typedef struct N { double v ; int s ; } *SN ;
typedef struct L { int v ; int s ; } *SL ;

typedef struct Block {
  SN prm ;
  SN inp1 ;
  SN inp2 ;
  SN inp3 ;
  SN out1 ;
  SN out2 ;
  SN out3 ;
  SL idx1 ;
  SL idx2 ;
  SL idx3 ;
  SN sum ;
} FB ;

/*@
  region A: fb ;
*/
void job(FB *fb)
{
  fb->out1->v = fb->out1->v + fb->out2->v ;
  fb->out1->s = fb->out1->s | fb->out2->s ;
}
