/* run.config
   OPT: -unspecified-access
*/
// Misra C Enforcement Testing
//
// Rule 12.2 Required
// The value of an expression shall be the same under any order of
// evaluation that the standard permits.
// 1 exp arithmetique qui n'est pas un appel de fct, &&, |-, ?: ni ","
// est evaluee dans un ordre indeterminé. On ne doit pas se baser sur l'ordre
// d'evaluation des termes de ces expressions
// 12.2.1: si un terme d'une exp est un operateur d'increment ou de decrement
// d'une var alors les autres termes ne doivent ni lire ni ecrire cette variable
// 12.2.2: l'ordre d'evaluation des args d'un appel de fct etant indefini, il
// faut que pour toute paire d'args (a,b) wr(a) inter rd(b)=0 et
// rd(a) inter wr(b)=0
// 12.2.3:
// 12.2.4:
// 12.2.5:
// 12.2.6:
///

typedef int SI_32;

static void func46 ( SI_32 m, SI_32 n ) ;

static SI_32 func46a ( SI_32 m, SI_32 n )
{
    return m + n;
}

static struct st
{
int st_m;
int st_n;
} local_st;

SI_32 main ( void )
{

  SI_32 i = 3;
  SI_32 x = 3;
  SI_32 y = 3;
  SI_32 z = 3;

  struct st this_st;

  this_st.st_m = 1;
  this_st.st_n = 2;

  z = ( y=i,++y ) + i++; // RULE 12.2.1: is est lu dans l'autre terme

  z = ++i + ( y=x,++y ) ; // y n'est PAS lu dans un autre terme

  z = ++i + ( y=i,++y ) ; // RULE 12.2.1: i est lu dans un autre terme

  z = ++i + ( 1 || i++ ) ;

  y = func46a ( x, ( x=3,x++ )  ) ; // RULE 12.2.2: x est lu dans le terme de G

  y = func46a ( x, ( i=2,i+3 )  ) ; // pas de conflits entre arguments effectifs

  z = i + i++; // RULE 12.2.1

  z = ( y=x,++y ) + i++;

  z = ( i = 3 ) + i + 8; // RULE 12.2.5

  z = ( this_st.st_m = 3 ) +
    this_st.st_m + 8; // RULE 12.2.5

  z = ( this_st.st_m = 3 ) +
    this_st.st_n + 8;

  z = ++i + (  ( y += 2,y ) ,y++ ) ; // pas de conflits

  z = (  ( ++i+i ) >0 ) ? ++i : --i;// RULE 12.2.1: conflits entre terme du +

  z = ( i>0 ) ? ++i : --i;

  z = ++i + ( 3*8*1 && i++ ) ; // RULE 12.2.1: conflits entre terme du +

  z = ++i + ( y, y++ ) ;

  z = ++i + ( 3*8*0 || i++ ) ; // RULE 12.2.1: idem

  z = ++i + ( i, y++ ) ; // le resultat de terme droit ne depend pas de i

  return z;

}
