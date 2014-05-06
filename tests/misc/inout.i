/* run.config
   GCC:
   OPT: -inout -deps -main inout_11_0 -journal-disable
   OPT: -inout -deps -main inout_11_3 -journal-disable
   OPT: -inout -deps -main never_terminate -journal-disable
   OPT: -inout -deps -main may_not_terminate -journal-disable
   OPT: -inout -deps -main call_may_not_terminate -journal-disable
*/

int Xt, Xs, Xs_I, Ys, Ys_I, Z, I;

void inout_11_0 (int i1, int i2, int *i) {
  Xs_I = Xs_I + 1;
  Xt = I ;
  Xs = i1 ;
  Ys = i1 + i2 ;

  *i = 0;
  Z = *i;
}


const int I_size=8;

const int Itab[8]={-40,-25,-15,-5,5,15,25,40};
int inout_11_3 (int i1, int es, int i2) {
  int r;
  es = i1 ;
  Xs = es ;

  if (i2 < Itab[0])
    r=-2;
  else
    if (i2>=Itab[I_size-1])
      r=-1;
   else
      for(Z=0;Z<I_size-1;Z++)
	{
	  if ((i2>=Itab[Z])&&(i2<Itab[Z+1]))
	    r=Z;
	}
  return r;
}

void never_terminate (int i1_nt, int i2_nt, int i3_nt, int es, int e) {
  Xs = i1_nt;
  es = i2_nt ;
  Xs = es ;
  Xs = i3_nt ;
  while (1) ;
  Z = e ;
}

int I5_nt ;
void may_not_terminate (int i1, int i2, int i3, int i4, int i5_nt, int es, int e) {
  Xs = i1;
  es = i2 ;
  if (i4) {
    Xs = i5_nt + I5_nt ;
    while (1) ;
    Z = e ;
    }
  Xs = es ;
  Xs = i3 ;
}

void call_may_not_terminate (int j1, int j2, int j3, int j4, int j5, int c1, int c2) {
  may_not_terminate(j1, j2, j3, j4, j5, c1, c2) ;
}
