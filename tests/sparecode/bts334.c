/*run.config
 OPT: -main main_init -sparecode-analysis -sparecode-no-annot  -journal-disable
 OPT: -main main_init -slice-pragma loop_body -journal-disable -then-on 'Slicing export' -print
 OPT: -main main_init -slice-pragma loop_body -calldeps -journal-disable -then-on 'Slicing export' -print
 */
int kf ;
int k[2] ;
int f(int vi , int i ) ;
static int si[2]  = {0, 0};
static int so[2]  = {0, 0};
int f(int vi , int i )
{ int vo ;

  {vo = so[i] / kf + k[i] * (vi - si[i]);
  so[i] = vo;
  si[i] = vi;
  return (vo);}

}
int volatile   e0 ;
int volatile   e1 ;
int s0 ;
int s1 ;
void loop_body(void)
{ int acq0 ;
  int acq1 ;
  int val0 ;
  int val1 ;

  {/*@ slice pragma expr s0;

   */
   ;
  ;
  acq0 = (int )e0;

  acq1 = (int )e1;
  val0 = f(acq0, 0);
  val1 = f(acq1, 1);
  s0 = val0;
  s1 = val1;
  return;}

}
int kf ;
int k[2] ;
void process(int conf )
{

  {kf = conf;
  k[0] = 3;
  k[1] = 14;
  while (1) {loop_body();}

  return;}

}
/*@ behavior default:
      assigns *p \from \nothing;
      */
extern int init(int *p ) ;
void main_init(void)
{ int is_ok ;
  int config ;

  {config = init(& is_ok);
  if (is_ok) {process(config);}

  return;}

}
