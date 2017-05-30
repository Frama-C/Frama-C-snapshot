/* run.config*
   STDOPT: #"-unspecified-access"
*/



int uppol2(int al1,int al2,int plt,int plt1,int plt2)
{
    long int wd2,wd4;
    int apl2;

    wd2 = 4L*(long)al1;
    if((long)plt*plt1 >= 0L) wd2 = -wd2;    /* check same sign */             /* CONDITION */
    wd2 = wd2 >> 7;                  /* gain of 1/128 */
//    Frama_C_show_each_TEST(plt,plt2,(long)plt*plt2>= 0L);
    if((long)plt*plt2 >= 0L) {             /* CONDITION */
        wd4 = wd2 + 128;             /* same sign case */
    }
    else {
        wd4 = wd2 - 128;
    }
    apl2 = wd4 + (127L*(long)al2 >> 7L);  /* leak factor of 127/128 */
    Frama_C_show_each_GOT(wd4);
    return(apl2);
}

int G;
void main() {
  G += uppol2(0,0,0,0,0);
  G += uppol2(0,0,-1,1,0);
  G += uppol2(0,0,-1,2,2);
  G += uppol2(0,0,0,3,0);
}
