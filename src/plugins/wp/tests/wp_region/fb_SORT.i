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
  region Shared: *(fb->inp1 .. fb->inp3);
  region IN:      (fb->inp1 .. fb->inp3);
  region OUT:     (fb->out1 .. fb->out3);
  region IDX:     (fb->idx1 .. fb->idx3);
 */
void job(FB *fb)
{
  SN *inp = &(fb->inp1) ;
  SN *out = &(fb->out1) ;
  SL *idx = &(fb->idx1) ;

  for (int i = 0; i < 3; i++) {
    out[i]->v = inp[i]->v + fb->prm->v ;
    out[i]->s = 0 ;
    idx[i]->v = inp[i]->s ;
    idx[i]->s = 0 ;
  }

  fb->sum->v =
    fb->out1->v +
    fb->out2->v +
    fb->out3->v ;

  fb->sum->s = 0 ;

}
