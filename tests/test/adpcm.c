/*************************************************************************/
/*                                                                       */
/*   SNU-RT Benchmark Suite for Worst Case Timing Analysis               */
/*   =====================================================               */
/*                              Collected and Modified by S.-S. Lim      */
/*                                           sslim@archi.snu.ac.kr       */
/*                                         Real-Time Research Group      */
/*                                        Seoul National University      */
/*                                                                       */
/*                                                                       */
/*        < Features > - restrictions for our experimental environment   */
/*                                                                       */
/*          1. Completely structured.                                    */
/*               - There are no unconditional jumps.                     */
/*               - There are no exit from loop bodies.                   */
/*                 (There are no 'break' or 'return' in loop bodies)     */
/*          2. No 'switch' statements.                                   */
/*          3. No 'do..while' statements.                                */
/*          4. Expressions are restricted.                               */
/*               - There are no multiple expressions joined by 'or',     */
/*                'and' operations.                                      */
/*          5. No library calls.                                         */
/*               - All the functions needed are implemented in the       */
/*                 source file.                                          */
/*                                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*  FILE: adpcm.c                                                        */
/*  SOURCE : C Algorithms for Real-Time DSP by P. M. Embree              */
/*                                                                       */
/*  DESCRIPTION :                                                        */
/*                                                                       */
/*     CCITT G.722 ADPCM (Adaptive Differential Pulse Code Modulation)   */
/*     algorithm.                                                        */
/*     16khz sample rate data is stored in the array test_data[SIZE].    */
/*     Results are stored in the array compressed[SIZE] and result[SIZE].*/
/*     Execution time is determined by the constant SIZE (default value  */
/*     is 2000).                                                         */
/*                                                                       */
/*  REMARK :                                                             */
/*                                                                       */
/*  EXECUTION TIME :                                                     */
/*                                                                       */
/*                                                                       */
/*************************************************************************/



/* common sampling rate for sound cards on IBM/PC */
/* PATHCRAWLER    ##define SAMPLE_RATE 11025 */

/* PATHCRAWLER    #define PI 3.14159265358979323846 */

/* COMPLEX STRUCTURE */

/* PATHCRAWLER    typedef struct {
    float real, imag;
    } COMPLEX; */

/* function prototypes for fft and filter functions */
/* PATHCRAWLER    void fft(COMPLEX *,int);
float fir_filter(float input,float *coef,int n,float *history);
float iir_filter(float input,float *coef,int n,float *history);
float gaussian(void);

void setup_codec(int),key_down(),int_enable(),int_disable();
int flags(int);

float getinput(void);
void sendout(float),flush(); */

int encode(int,int);
int filtez(int *bpl,int *dlt);
void upzero(int dlt,int *dlti,int *bli);
int filtep(int rlt1,int al1,int rlt2,int al2);
int quantl(int el,int detl);
int invqxl(int il,int detl,int *code_table,int mode);
int logscl(int il,int nbl);
int scalel(int nbl,int shift_constant);
int uppol2(int al1,int al2,int plt,int plt1,int plt2);
int uppol1(int al1,int apl2,int plt,int plt1);
int invqah(int ih,int deth);
int logsch(int ih,int nbh);

/* G722 C code */

/* variables for transimit quadrature mirror filter here */
int tqmf[24];

/* QMF filter coefficients:
scaled by a factor of 4 compared to G722 CCITT recomendation */
int h[24] = {
    12,   -44,   -44,   212,    48,  -624,   128,  1448,
  -840, -3220,  3804, 15504, 15504,  3804, -3220,  -840,
  1448,   128,  -624,    48,   212,   -44,   -44,    12
};

int xl,xh;

/* variables for receive quadrature mirror filter here */
int accumc[11],accumd[11];

/* outputs of decode() */
int xout1,xout2;

int xs,xd;

/* variables for encoder (hi and lo) here */

int il,szl,spl,sl,el;

int qq4_code4_table[16] = {
     0,  -20456,  -12896,   -8968,   -6288,   -4240,   -2584,   -1200,
 20456,   12896,    8968,    6288,    4240,    2584,    1200,       0
};

int qq5_code5_table[32] = {
  -280,    -280,  -23352,  -17560,  -14120,  -11664,   -9752,   -8184,
 -6864,   -5712,   -4696,   -3784,   -2960,   -2208,   -1520,    -880,
 23352,   17560,   14120,   11664,    9752,    8184,    6864,    5712,
  4696,    3784,    2960,    2208,    1520,     880,     280,    -280
};

int qq6_code6_table[64] = {
  -136,    -136,    -136,    -136,  -24808,  -21904,  -19008,  -16704,
-14984,  -13512,  -12280,  -11192,  -10232,   -9360,   -8576,   -7856,
 -7192,   -6576,   -6000,   -5456,   -4944,   -4464,   -4008,   -3576,
 -3168,   -2776,   -2400,   -2032,   -1688,   -1360,   -1040,    -728,
 24808,   21904,   19008,   16704,   14984,   13512,   12280,   11192,
 10232,    9360,    8576,    7856,    7192,    6576,    6000,    5456,
  4944,    4464,    4008,    3576,    3168,    2776,    2400,    2032,
  1688,    1360,    1040,     728,     432,     136,    -432,    -136
};

int delay_bpl[6];

int delay_dltx[6];

int wl_code_table[16] = {
   -60,  3042,  1198,   538,   334,   172,    58,   -30,
  3042,  1198,   538,   334,   172,    58,   -30,   -60
};

int wl_table[8] = {
   -60,   -30,    58,   172,   334,   538,  1198,  3042
};

int ilb_table[32] = {
  2048,  2093,  2139,  2186,  2233,  2282,  2332,  2383,
  2435,  2489,  2543,  2599,  2656,  2714,  2774,  2834,
  2896,  2960,  3025,  3091,  3158,  3228,  3298,  3371,
  3444,  3520,  3597,  3676,  3756,  3838,  3922,  4008
};

int         nbl;                  /* delay line */
int         al1,al2;
int         plt,plt1,plt2;
int         rs;
int         dlt;
int         rlt,rlt1,rlt2;

/* decision levels - pre-multiplied by 8, 0 to indicate end */
int decis_levl[30] = {
   280,   576,   880,  1200,  1520,  1864,  2208,  2584,
  2960,  3376,  3784,  4240,  4696,  5200,  5712,  6288,
  6864,  7520,  8184,  8968,  9752, 10712, 11664, 12896,
 14120, 15840, 17560, 20456, 23352, 32767
};

int         detl;

/* quantization table 31 long to make quantl look-up easier,
last entry is for mil=30 case when wd is max */
int quant26bt_pos[31] = {
    61,    60,    59,    58,    57,    56,    55,    54,
    53,    52,    51,    50,    49,    48,    47,    46,
    45,    44,    43,    42,    41,    40,    39,    38,
    37,    36,    35,    34,    33,    32,    32
};

/* quantization table 31 long to make quantl look-up easier,
last entry is for mil=30 case when wd is max */
int quant26bt_neg[31] = {
    63,    62,    31,    30,    29,    28,    27,    26,
    25,    24,    23,    22,    21,    20,    19,    18,
    17,    16,    15,    14,    13,    12,    11,    10,
     9,     8,     7,     6,     5,     4,     4
};


int         deth;
int         sh;         /* this comes from adaptive predictor */
int         eh;

int qq2_code2_table[4] = {
  -7408,   -1616,   7408,  1616
};

int wh_code_table[4] = {
   798,   -214,    798,   -214
};


int         dh,ih;
int         nbh,szh;
int         sph,ph,yh,rh;

int         delay_dhx[6];

int         delay_bph[6];

int         ah1,ah2;
int         ph1,ph2;
int         rh1,rh2;

/* variables for decoder here */
int         ilr,yl,rl;
int         dec_deth,dec_detl,dec_dlt;

int         dec_del_bpl[6];

int         dec_del_dltx[6];

int     dec_plt,dec_plt1,dec_plt2;
int     dec_szl,dec_spl,dec_sl;
int     dec_rlt1,dec_rlt2,dec_rlt;
int     dec_al1,dec_al2;
int     dl;
int     dec_nbl,dec_yh,dec_dh,dec_nbh;

/* variables used in filtez */
int         dec_del_bph[6];

int         dec_del_dhx[6];

int         dec_szh;
/* variables used in filtep */
int         dec_rh1,dec_rh2;
int         dec_ah1,dec_ah2;
int         dec_ph,dec_sph;

int     dec_sh,dec_rh;

int     dec_ph1,dec_ph2;

/* G722 encode function two ints in, one 8 bit output */

/* put input samples in xin1 = first value, xin2 = second value */
/* returns il and ih stored together */
int abs(int x )
{ int m ;

  if (x >= 0) {
    m = x;
  } else {
    m = - x;
  }
  return (m);
}

/*@ ensures sh == 0 || sh == -2; */
int encode(int xin1,int xin2)
{
    int i;
    int *h_ptr,*tqmf_ptr,*tqmf_ptr1;
    long int xa,xb;
    int decis;

/* transmit quadrature mirror filters implemented here */
    h_ptr = h;
    tqmf_ptr = tqmf;
    xa = (long)(*tqmf_ptr++) * (*h_ptr++);
    xb = (long)(*tqmf_ptr++) * (*h_ptr++);
/*@ loop pragma UNROLL 11; */
/* main multiply accumulate loop for samples and coefficients */
    for(i = 0 ; i < 10 ; i++) {
        xa += (long)(*tqmf_ptr++) * (*h_ptr++);
        xb += (long)(*tqmf_ptr++) * (*h_ptr++);
    }
/* final mult/accumulate */
    xa += (long)(*tqmf_ptr++) * (*h_ptr++);
    xb += (long)(*tqmf_ptr) * (*h_ptr++);

/* update delay line tqmf */
    tqmf_ptr1 = tqmf_ptr - 2;
/*@ loop pragma UNROLL 23; */
    for(i = 0 ; i < 22 ; i++) *tqmf_ptr-- = *tqmf_ptr1--;
    *tqmf_ptr-- = xin1;
    *tqmf_ptr = xin2;

/* scale outputs */
    xl = (xa + xb) >> 15;
    xh = (xa - xb) >> 15;

/* end of quadrature mirror filter code */

/* starting with lower sub band encoder */

/* filtez - compute predictor output section - zero section */
    szl = filtez(delay_bpl,delay_dltx);

/* filtep - compute predictor output signal (pole section) */
    spl = filtep(rlt1,al1,rlt2,al2);

/* compute the predictor output value in the lower sub_band encoder */
    sl = szl + spl;
    el = xl - sl;

/* quantl: quantize the difference signal */
    il = quantl(el,detl);             /* CONDITIONS: 30 x 4 possibilities */

/* invqxl: computes quantized difference signal */
/* for invqbl, truncate by 2 lsbs, so mode = 3 */
    dlt = ((long)detl*qq4_code4_table[il >> 2]) >> 15;

/* logscl: updates logarithmic quant. scale factor in low sub band */
    nbl = logscl(il,nbl);             /* CONDITIONS: 2 possibs */

/* scalel: compute the quantizer scale factor in the lower sub band */
/* calling parameters nbl and 8 (constant such that scalel can be scaleh) */
    detl = scalel(nbl,8);

/* parrec - simple addition to compute recontructed signal for adaptive pred */
    plt = dlt + szl;

/* upzero: update zero section predictor coefficients (sixth order)*/
/* calling parameters: dlt, dlt1, dlt2, ..., dlt6 from dlt */
/*  bpli (linear_buffer in which all six values are delayed */
/* return params:      updated bpli, delayed dltx */
    upzero(dlt,delay_dltx,delay_bpl);  /* CONDITIONS: 2exp6 possibs */

/* uppol2- update second predictor coefficient apl2 and delay it as al2 */
/* calling parameters: al1, al2, plt, plt1, plt2 */
    al2 = uppol2(al1,al2,plt,plt1,plt2);  /* CONDITION: 2 possibs */

/* uppol1 :update first predictor coefficient apl1 and delay it as al1 */
/* calling parameters: al1, apl2, plt, plt1 */
    al1 = uppol1(al1,al2,plt,plt1);      /* CONDITIONS: 8 possibs */

/* recons : compute recontructed signal for adaptive predictor */
    rlt = sl + dlt;

/* done with lower sub_band encoder; now implement delays for next time*/
    rlt2 = rlt1;
    rlt1 = rlt;
    plt2 = plt1;
    plt1 = plt;

/* high band encode */

    szh = filtez(delay_bph,delay_dhx);

    sph = filtep(rh1,ah1,rh2,ah2);

/* predic: sh = sph + szh */
    sh = sph + szh;
/* subtra: eh = xh - sh */
    eh = xh - sh;

/* quanth - quantization of difference signal for higher sub-band */
/* quanth: in-place for speed params: eh, deth (has init. value) */
    if(eh >= 0) {             /* CONDITION */
        ih = 3;     /* 2,3 are pos codes */
    }
    else {
        ih = 1;     /* 0,1 are neg codes */
    }
    decis = (564L*(long)deth) >> 12L;
    if(abs(eh) > decis) ih--;     /* mih = 2 case */             /* CONDITIONS: 2 possibs */

/* invqah: compute the quantized difference signal, higher sub-band*/
    dh = ((long)deth*qq2_code2_table[ih]) >> 15L ;

/* logsch: update logarithmic quantizer scale factor in hi sub-band*/
    nbh = logsch(ih,nbh);

/* note : scalel and scaleh use same code, different parameters */
    deth = scalel(nbh,10);

/* parrec - add pole predictor output to quantized diff. signal */
    ph = dh + szh;

/* upzero: update zero section predictor coefficients (sixth order) */
/* calling parameters: dh, dhi, bphi */
/* return params: updated bphi, delayed dhx */
    upzero(dh,delay_dhx,delay_bph);  /* CONDITIONS: 2exp6 possibs */

/* uppol2: update second predictor coef aph2 and delay as ah2 */
/* calling params: ah1, ah2, ph, ph1, ph2 */
    ah2 = uppol2(ah1,ah2,ph,ph1,ph2);  /* CONDITION: 2 possibs */

/* uppol1:  update first predictor coef. aph2 and delay it as ah1 */
    ah1 = uppol1(ah1,ah2,ph,ph1);      /* CONDITIONS: 8 possibs */

/* recons for higher sub-band */
    yh = sh + dh;

/* done with higher sub-band encoder, now Delay for next time */
    rh2 = rh1;
    rh1 = yh;
    ph2 = ph1;
    ph1 = ph;

/* multiplex ih and il to get signals together */
    return(il | (ih << 6));
}

/* filtez - compute predictor output signal (zero section) */
/* input: bpl1-6 and dlt1-6, output: szl */

int filtez(int *bpl,int *dlt)
{
    int i;
    long int zl;
    zl = (long)(*bpl++) * (*dlt++);
/*@ loop pragma UNROLL 7; */
    for(i = 1 ; i < 6 ; i++)
        zl += (long)(*bpl++) * (*dlt++);

    return((int)(zl >> 14));   /* x2 here */
}

/* filtep - compute predictor output signal (pole section) */
/* input rlt1-2 and al1-2, output spl */

int filtep(int rlt1,int al1,int rlt2,int al2)
{
    long int pl,pl2;
    pl = 2*rlt1;
    pl = (long)al1*pl;
    pl2 = 2*rlt2;
    pl += (long)al2*pl2;
    return((int)(pl >> 15));
}

/* quantl - quantize the difference signal in the lower sub-band */
int quantl(int el,int detl)
{
    int ril,mil;
    long int wd,decis;

/* abs of difference signal */
    wd = abs(el);
/* determine mil based on decision levels and detl gain */
/*    for(mil = 0 ; mil < 30 ; mil++) {
        decis = (decis_levl[mil]*(long)detl) >> 15L;
        if(wd <= decis) break;
        }*/
    mil = 0;
    decis = (decis_levl[mil]*(long)detl) >> 15L;
/*@ loop pragma UNROLL 30; */
    while(wd <= decis && mil < 29) {             /* FOR/BREAK 662 : 30 possibilities */
      mil++;
      decis = (decis_levl[mil]*(long)detl) >> 15L;
      }
/* if mil=30 then wd is less than all decision levels */
    if(el >= 0) ril = quant26bt_pos[mil];             /* CONDITION 665 */
    else ril = quant26bt_neg[mil];
    return(ril);
}

/* invqxl is either invqbl or invqal depending on parameters passed */
/* returns dlt, code table is pre-multiplied by 8 */

int invqxl(int il,int detl,int *code_table,int mode)
{
    long int dlt;
    dlt = (long)detl*code_table[il >> (mode-1)];
    return((int)(dlt >> 15));
}

/* logscl - update log quantizer scale factor in lower sub-band */
/* note that nbl is passed and returned */

int logscl(int il,int nbl)
{
    long int wd;
    wd = ((long)nbl * 127L) >> 7L;   /* leak factor 127/128 */
    nbl = (int)wd + wl_code_table[il >> 2];
    if(nbl < 0) nbl = 0;             /* CONDITION */
    if(nbl > 18432) nbl = 18432;             /* CONDITION */
    return(nbl);
}

/* scalel: compute quantizer scale factor in lower or upper sub-band*/

int scalel(int nbl,int shift_constant)
{
    int wd1,wd2,wd3;
    wd1 = (nbl >> 6) & 31;
    wd2 = nbl >> 11;
    wd3 = ilb_table[wd1] >> (shift_constant + 1 - wd2);
    return(wd3 << 3);
}

/* upzero - inputs: dlt, dlti[0-5], bli[0-5], outputs: updated bli[0-5] */
/* also implements delay of bli and update of dlti from dlt */

void upzero(int dlt,int *dlti,int *bli)
{
    int i,wd2,wd3;
/*if dlt is zero, then no sum into bli */
    if(dlt == 0) {             /* CONDITION 711 */
/*@ loop pragma UNROLL 7; */
      for(i = 0 ; i < 6 ; i++) {
        bli[i] = (int)((255L*bli[i]) >> 8L); /* leak factor of 255/256 */
      }
    }
    else {
/*@ loop pragma UNROLL 7; */
      for(i = 0 ; i < 6 ; i++) {
        if((long)dlt*dlti[i] >= 0) wd2 = 128; else wd2 = -128;   /* CONDITION 718 : 2exp6 possibs */
        wd3 = (int)((255L*bli[i]) >> 8L);    /* leak factor of 255/256 */
        bli[i] = wd2 + wd3;
      }
    }
/* implement delay line for dlt */
    dlti[5] = dlti[4];
    dlti[4] = dlti[3];
    dlti[3] = dlti[2];
    dlti[1] = dlti[0];
    dlti[0] = dlt;
}

/* uppol2 - update second predictor coefficient (pole section) */
/* inputs: al1, al2, plt, plt1, plt2. outputs: apl2 */

int uppol2(int al1,int al2,int plt,int plt1,int plt2)
{
    long int wd2,wd4;
    int apl2;
    wd2 = 4L*(long)al1;
    if((long)plt*plt1 >= 0L) wd2 = -wd2;    /* check same sign */             /* CONDITION */
    wd2 = wd2 >> 7;                  /* gain of 1/128 */
    if((long)plt*plt2 >= 0L) {             /* CONDITION */
        wd4 = wd2 + 128;             /* same sign case */
    }
    else {
        wd4 = wd2 - 128;
    }
    apl2 = wd4 + (127L*(long)al2 >> 7L);  /* leak factor of 127/128 */

/* apl2 is limited to +-.75 */
    if(apl2 > 12288) apl2 = 12288;             /* CONDITION */
    if(apl2 < -12288) apl2 = -12288;             /* CONDITION */
    return(apl2);
}

/* uppol1 - update first predictor coefficient (pole section) */
/* inputs: al1, apl2, plt, plt1. outputs: apl1 */

int uppol1(int al1,int apl2,int plt,int plt1)
{
    long int wd2;
    int wd3,apl1;
    wd2 = ((long)al1*255L) >> 8L;   /* leak factor of 255/256 */
    if((long)plt*plt1 >= 0L) {             /* CONDITION */
        apl1 = (int)wd2 + 192;      /* same sign case */
    }
    else {
        apl1 = (int)wd2 - 192;
    }
/* note: wd3= .9375-.75 is always positive */
    wd3 = 15360 - apl2;             /* limit value */
    if(apl1 > wd3) apl1 = wd3;             /* CONDITION */
    if(apl1 < -wd3) apl1 = -wd3;             /* CONDITION */
    return(apl1);
}

/* INVQAH: inverse adaptive quantizer for the higher sub-band */
/* returns dh, code table is pre-multiplied by 8 */

int invqah(int ih,int deth)
{
    long int rdh;
    rdh = ((long)deth*qq2_code2_table[ih]) >> 15L ;
    return((int)(rdh ));
}

/* logsch - update log quantizer scale factor in higher sub-band */
/* note that nbh is passed and returned */

int logsch(int ih,int nbh)
{
    int wd;
    wd = ((long)nbh * 127L) >> 7L;       /* leak factor 127/128 */
    nbh = wd + wh_code_table[ih];
    if(nbh < 0) nbh = 0;             /* CONDITION */
    if(nbh > 22528) nbh = 22528;             /* CONDITION */
    return(nbh);
}


/* PATHCRAWLER    #define SIZE 2000
   #define IN_END 2000 */

/* PATHCRAWLER    #define PI 3.14159265358979323846 */

/* PATHCRAWLER    void main() */
int test_data[10]={1,0,1,0,1,1,1,1,1,1};
int compressed[10]={0};
void main () //(int test_data[10], int compressed[10])
{
  int i;
/*@ loop pragma UNROLL 11; */
  for(i = 0 ; i < 10 ; i += 2)
    compressed[i/2] = encode(test_data[i],test_data[i+1]);

}
