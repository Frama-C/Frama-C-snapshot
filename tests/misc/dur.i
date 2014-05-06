/* run.config
  GCC:
  OPT: -float-normal -val -deps -out -input  -main F2 -journal-disable
*/

struct T1 {
   float M1 ;
   unsigned short M2 ;
   unsigned short M3 ;
};
typedef struct T1 T2;
struct T3 {
   unsigned short M4 ;
   unsigned short M5 ;
};
typedef struct T3 T4;
struct T5 {
   float M6 ;
   float M7 ;
   float M8 ;
   float M9 ;
   float M10 ;
   float M11 ;
   float M12 ;
   float M13 ;
   float M14 ;
   float M15 ;
   float M16 ;
   float M17 ;
   float M18 ;
   float M19 ;
   float M20 ;
   float M21 ;
   float M22 ;
   float M23 ;
   float M24 ;
   float M25 ;
   float M26[(unsigned short)26] ;
   float M27[(unsigned short)13] ;
   float M28[(unsigned short)3] ;
   float M29 ;
   float M30 ;
   float M31 ;
   float M32 ;
   float M33 ;
   float M34 ;
   float M35 ;
   float M36 ;
   float M37 ;
   float M38 ;
   float M39 ;
   float M40 ;
   float M41 ;
   float M42 ;
   float M43 ;
   float M44 ;
   float M45 ;
   float M46 ;
   float M47 ;
   float M48 ;
   float M49 ;
   float M50 ;
   float M51 ;
   float M52 ;
   float M53 ;
   float M54 ;
   float M55 ;
   float M56 ;
   float M57 ;
   float M58 ;
   float M59 ;
   float M60 ;
   float M61 ;
   float M62 ;
   float M63 ;
   float M64[27] ;
   float M65[27] ;
   float M66[(unsigned short)48] ;
   float M67[(unsigned short)48] ;
   float M68[(unsigned short)48] ;
   float M69[(unsigned short)48] ;
   float M70[48] ;
   float M71[48] ;
   float M72[48] ;
   float M73[48] ;
   float M74[(unsigned short)10] ;
};
typedef struct T5 T6;
struct T7 {
   unsigned short M75 ;
   T2 M76[(unsigned short)53] ;
   T2 M77 ;
   T2 M78 ;
   T2 M79 ;
   T2 M80 ;
   T2 M81 ;
   T2 M82 ;
   T2 M83 ;
   T2 M84 ;
   T2 M85 ;
   T2 M86 ;
   T2 M87 ;
   T2 M88 ;
   T2 M89 ;
   T4 M90[(unsigned short)4] ;
   T4 M91 ;
   T2 M92[(unsigned short)6] ;
   T4 M93[(unsigned short)5] ;
};
typedef struct T7 T8;
struct T9 {
   unsigned short M94[(unsigned short)1][16] ;
   unsigned short M95[(unsigned short)1] ;
   unsigned short M96[(unsigned short)1] ;
   unsigned short M97[(unsigned short)1] ;
   unsigned short M98 ;
};
typedef struct T9 T10;
int G1 ;
int G2 ;
extern unsigned char G3 ;
extern T6 const   G4 ;
extern T8 G5 ;
extern T10 G6 ;
extern unsigned char G7[(unsigned short)161] ;
void F1(T2 *V1 , T2 *V2 , unsigned short const   V3 ,
        unsigned short const   V4 )
{

  {if ((int )V1->M2 != 0)
   {if ((int )V1->M2 == 2) {G7[V3] = (unsigned char)1;}
    else {G7[V3] = (unsigned char)0;}

   V1->M2 = (unsigned short)1;
   if ((int )V2->M2 == 0)
   {G7[V4] = (unsigned char)0;
   if (V2->M1 <= G4.M16) {G7[V3] = (unsigned char)1;
     if (V2->M1 <= G4.M17) {G7[V4] = (unsigned char)1;
       V2->M2 = (unsigned short)1;}
     }
   }
   else {G7[V4] = (unsigned char)1;
   V2->M2 = (unsigned short)1;}
   }
   else {G7[V3] = (unsigned char)0;
   V2->M2 = (unsigned short )((int )V2->M2 != 0);
   G7[V4] = (unsigned char )V2->M2;}

  return;}

}
void F2(unsigned short V8 )
{ unsigned short V5 ;
  unsigned short V6 ;
  unsigned short V7 ;

  {G5.M75 = (unsigned short )G3;
  if ((int )V8 == 0) {if ((((int )G6.M97[0] & 1) == 1) == 1)
                      {G5.M91.M4 = (unsigned short)0;
                      G5.M91.M5 = (unsigned short)1;}
                      else {G5.M91.M4 = (unsigned short )(((int )G6.M96[0] & 1) == 1);
                      G5.M91.M5 = (unsigned short)0;}

    V6 = (unsigned short)0;
    V7 = (unsigned short)2;
    V5 = (unsigned short)0;
    while ((int )V5 < 4) {if (G2)
                          {G5.M90[V5].M4 = (unsigned short)0;
                          G5.M90[V5].M5 = (unsigned short)1;}
                          else {G5.M90[V5].M4 = (unsigned short )G1;
                          if ((int )G5.M90[V5].M4 == 1) {V6 = (unsigned short )(
                                                         (int )V6 + 1);}

                          G5.M90[V5].M5 = (unsigned short)0;}

      V7 = (unsigned short )(2 * (int )V7);
      V5 = (unsigned short )((int )V5 + 1);}
    }

  return;}

}
