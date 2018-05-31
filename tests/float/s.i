/* run.config*
   STDOPT: #"-float-hex"

*/

typedef float T1;
typedef int T2;
typedef int T3;
extern int F1(int G1 ) ;
extern int F2(int G2 ) ;
int const   G3  = (int const   )42;
extern int F3(int G4 ) ;
T3 G5[64] ;
int const   G6  = (int const   )42;
int G7 ;
T2 G8 ;
T1 const   G9[64]  =   
                       {(T1 const   )2.000f, (T1 const   )1.882f,
                        (T1 const   )1.778f, (T1 const   )1.684f,
                        (T1 const   )1.600f, (T1 const   )1.523f,
                        (T1 const   )1.455f, (T1 const   )1.391f,
                        (T1 const   )1.333f, (T1 const   )1.280f,
                        (T1 const   )1.231f, (T1 const   )1.185f,
                        (T1 const   )1.143f, (T1 const   )1.063f,
                        (T1 const   )1.000f, (T1 const   )0.944f,
                        (T1 const   )0.895f, (T1 const   )0.850f,
                        (T1 const   )0.810f, (T1 const   )0.773f,
                        (T1 const   )0.739f, (T1 const   )0.708f,
                        (T1 const   )0.680f, (T1 const   )0.654f,
                        (T1 const   )0.630f, (T1 const   )0.607f,
                        (T1 const   )0.586f, (T1 const   )0.567f,
                        (T1 const   )0.548f, (T1 const   )0.500f,
                        (T1 const   )0.471f, (T1 const   )0.444f,
                        (T1 const   )0.421f, (T1 const   )0.400f,
                        (T1 const   )0.381f, (T1 const   )0.364f,
                        (T1 const   )0.348f, (T1 const   )0.333f,
                        (T1 const   )0.320f, (T1 const   )0.308f,
                        (T1 const   )0.296f, (T1 const   )0.286f,
                        (T1 const   )0.276f, (T1 const   )0.267f,
                        (T1 const   )0.258f, (T1 const   )0.250f,
                        (T1 const   )0.236f, (T1 const   )0.222f,
                        (T1 const   )0.211f, (T1 const   )0.200f,
                        (T1 const   )0.190f, (T1 const   )0.182f,
                        (T1 const   )0.174f, (T1 const   )0.167f,
                        (T1 const   )0.160f, (T1 const   )0.154f,
                        (T1 const   )0.148f, (T1 const   )0.143f,
                        (T1 const   )0.138f, (T1 const   )0.133f,
                        (T1 const   )0.129f, (T1 const   )0.125f,
                        (T1 const   )0.118f, (T1 const   )0.111f};
T1 const   G10[64]  =   
                        {(T1 const   )0.0510143148127383f,
                         (T1 const   )0.0526807976019492f,
                         (T1 const   )0.0547630669950585f,
                         (T1 const   )0.0564281924367408f,
                         (T1 const   )0.0585087059708387f,
                         (T1 const   )0.0605881929148253f,
                         (T1 const   )0.0630821707769080f,
                         (T1 const   )0.0655745547964065f,
                         (T1 const   )0.0680652820004121f,
                         (T1 const   )0.0713835655737245f,
                         (T1 const   )0.0742844385649674f,
                         (T1 const   )0.0780103647018580f,
                         (T1 const   )0.0817318560546706f,
                         (T1 const   )0.0862740143728233f,
                         (T1 const   )0.0908088461410527f,
                         (T1 const   )0.0961582204249914f,
                         (T1 const   )0.1023163627594810f,
                         (T1 const   )0.1055941606958780f,
                         (T1 const   )0.1088672156067600f,
                         (T1 const   )0.1125435499383810f,
                         (T1 const   )0.1166208530287330f,
                         (T1 const   )0.1210964219228140f,
                         (T1 const   )0.1255617070832910f,
                         (T1 const   )0.1304207531449480f,
                         (T1 const   )0.1356698301501220f,
                         (T1 const   )0.1417064222370730f,
                         (T1 const   )0.1481207659913060f,
                         (T1 const   )0.1549068205428340f,
                         (T1 const   )0.1624539893402380f,
                         (T1 const   )0.1707476333220120f,
                         (T1 const   )0.1797704190221740f,
                         (T1 const   )0.1902769765139430f,
                         (T1 const   )0.2014532178568310f,
                         (T1 const   )0.2079499479102800f,
                         (T1 const   )0.2144044999107600f,
                         (T1 const   )0.2215668881520150f,
                         (T1 const   )0.2290455646566350f,
                         (T1 const   )0.2371977623669650f,
                         (T1 const   )0.2452693886855440f,
                         (T1 const   )0.2543404121628250f,
                         (T1 const   )0.2640116929021380f,
                         (T1 const   )0.2745989285199270f,
                         (T1 const   )0.2860443073244380f,
                         (T1 const   )0.2982805530233570f,
                         (T1 const   )0.3112299993821370f,
                         (T1 const   )0.3254401125978740f,
                         (T1 const   )0.3407438063507600f,
                         (T1 const   )0.3572363398164020f,
                         (T1 const   )0.3749169970825380f,
                         (T1 const   )0.3841864461394160f,
                         (T1 const   )0.3939193988832260f,
                         (T1 const   )0.4037685876851210f,
                         (T1 const   )0.4141279920488020f,
                         (T1 const   )0.4243935880669410f,
                         (T1 const   )0.4350918773347630f,
                         (T1 const   )0.4457881609076350f,
                         (T1 const   )0.4562601664070490f,
                         (T1 const   )0.4665646755216170f,
                         (T1 const   )0.4764261613996570f,
                         (T1 const   )0.4852153448066030f,
                         (T1 const   )0.4927158273036620f,
                         (T1 const   )0.4979548345880140f,
                         (T1 const   )0.4999998245403760f,
                         (T1 const   )0.4973478192101480f};
T1 const   G11[64]  =   
                        {(T1 const   )1.98956292560627f,
                         (T1 const   )1.98886795364206f,
                         (T1 const   )1.98796783271076f,
                         (T1 const   )1.98722262104002f,
                         (T1 const   )1.98625972652367f,
                         (T1 const   )1.98526198121786f,
                         (T1 const   )1.98401870850080f,
                         (T1 const   )1.98272530730105f,
                         (T1 const   )1.98138181029787f,
                         (T1 const   )1.97951261300748f,
                         (T1 const   )1.97780412452634f,
                         (T1 const   )1.97550756211798f,
                         (T1 const   )1.97309869476763f,
                         (T1 const   )1.97000221093885f,
                         (T1 const   )1.96673843085462f,
                         (T1 const   )1.96266592835235f,
                         (T1 const   )1.95767765236944f,
                         (T1 const   )1.95489078253260f,
                         (T1 const   )1.95201610389272f,
                         (T1 const   )1.94867729238996f,
                         (T1 const   )1.94483758350707f,
                         (T1 const   )1.94045626221254f,
                         (T1 const   )1.93591015375954f,
                         (T1 const   )1.93076327766655f,
                         (T1 const   )1.92496731270757f,
                         (T1 const   )1.91799599539423f,
                         (T1 const   )1.91022611722130f,
                         (T1 const   )1.90159460221914f,
                         (T1 const   )1.89149126922623f,
                         (T1 const   )1.87976698860229f,
                         (T1 const   )1.86625870208647f,
                         (T1 const   )1.84951743850643f,
                         (T1 const   )1.83048234524184f,
                         (T1 const   )1.81882080113072f,
                         (T1 const   )1.80679034940091f,
                         (T1 const   )1.79291099198876f,
                         (T1 const   )1.77781047048834f,
                         (T1 const   )1.76062362373384f,
                         (T1 const   )1.74283872793243f,
                         (T1 const   )1.72191035650916f,
                         (T1 const   )1.69845965986100f,
                         (T1 const   )1.67138471193539f,
                         (T1 const   )1.64038363438451f,
                         (T1 const   )1.60513531735156f,
                         (T1 const   )1.56530322933083f,
                         (T1 const   )1.51836086942351f,
                         (T1 const   )1.46365929605818f,
                         (T1 const   )1.39932668102673f,
                         (T1 const   )1.32325202617558f,
                         (T1 const   )1.28000484125813f,
                         (T1 const   )1.23176301086518f,
                         (T1 const   )1.17963335048658f,
                         (T1 const   )1.12069982565629f,
                         (T1 const   )1.05746929909226f,
                         (T1 const   )0.98545468309658f,
                         (T1 const   )0.90574093951495f,
                         (T1 const   )0.81806269246518f,
                         (T1 const   )0.71908167608869f,
                         (T1 const   )0.60686885217797f,
                         (T1 const   )0.48275988506435f,
                         (T1 const   )0.34014381721778f,
                         (T1 const   )0.18070894655987f,
                         (T1 const   )(- 0.00167551588592f),
                         (T1 const   )(- 0.20572395978728f)};
T1 const   G12[32]  =   
                        {(T1 const   )0.666666666666667f,
                         (T1 const   )0.592592592592593f,
                         (T1 const   )0.533333333333333f,
                         (T1 const   )0.484848484848485f,
                         (T1 const   )0.444444444444444f,
                         (T1 const   )0.410256410256410f,
                         (T1 const   )0.380952380952381f,
                         (T1 const   )0.355555555555556f,
                         (T1 const   )0.333333333333333f,
                         (T1 const   )0.296296296296296f,
                         (T1 const   )0.266666666666667f,
                         (T1 const   )0.242424242424242f,
                         (T1 const   )0.222222222222222f,
                         (T1 const   )0.205128205128205f,
                         (T1 const   )0.190476190476191f,
                         (T1 const   )0.177777777777778f,
                         (T1 const   )0.166666666666667f,
                         (T1 const   )0.148148148148148f,
                         (T1 const   )0.133333333333333f,
                         (T1 const   )0.121212121212121f,
                         (T1 const   )0.111111111111111f,
                         (T1 const   )0.102564102564103f,
                         (T1 const   )0.095238095238095f,
                         (T1 const   )0.088888888888889f,
                         (T1 const   )0.083333333333333f,
                         (T1 const   )0.074074074074074f,
                         (T1 const   )0.066666666666667f,
                         (T1 const   )0.060606060606061f,
                         (T1 const   )0.055555555555556f,
                         (T1 const   )0.051282051282051f,
                         (T1 const   )0.047619047619048f,
                         (T1 const   )0.044444444444445f};
static T2 G13 ;
static T1 G14 ;
static T1 G15 ;
static T1 G16 ;
static T1 G17 ;
static T1 G18 ;
static T1 G19 ;
void F4(void) 
{ T1 V1 ;
  T1 V2 ;
  T1 V3 ;
  int V4 ;
  int V5 ;
  int V6 ;
  int V7 ;
  
  {{V1 = (float )0.0;
   V4 = F1(G13);
   G16 = (float )G9[V4];}
  
  {V5 = F2(G13);
/* JLCo
  G14 = (float )(G10[V5] / (T1 const   )((float )G3));
  G14 = (float )(G10[V5] / (T1 const   )G3);
*/
  G14 = (float )(G10[V5] / G3);
}
  
  {V6 = F2(G13);
  G15 = (float )G11[V6];}
  
  {V7 = F3(G13);
  G17 = (float )G12[V7];}
  
  {G18 = (float )(1.0 / ((double )G14 + 1.0));
  V2 = G15 * G18;}
  
  {V3 = (float )(((double )G14 - 1.0) * (double )G18);
  G19 = (G16 * G14) * G18;}
  
  
  return;}

}
int main(void) 
{ int V8 ;
  
  {F4();
  V8 = 0;
  return (V8);}

}

