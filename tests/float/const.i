/* run.config
   OPT: -memory-footprint 1 -val -out -deps -float-hex -journal-disable
*/
typedef double mydouble;

float f0, f_ , f00, f1 = 3.0, f2, f3, f_0, f13, f26, fic0,fic1,fic2,fic4, fec0,fec2,fec4;

mydouble m0, m_ , m00, m1 = 3.0, m2, m3, m_0, m13, m26;

double d0, d1 = 3.0, d2, d3, d4, d5, d6, d7;

int A,B,C,D,E,F,G,H,I,J,K,L,P,Q,R;

int Am,Bm,Cm,Dm,Em,Fm,Gm,Hm,Im,Jm,Km,Lm;

int t1,t2,t3,t4,t5,t6,t7,t8,t9,C0=0,C2=2;
int s1,s2,s3,s4,s5,s6,s7,s8,s9;
int if1,if2,if3,ite1,ite2,ite3;
int ca1,ca2,ca3,ca4;

void main(int c1, int c2)
{
  f_ = - f0;
  f_0 = c1 ? f0 : f_;
  f00 = - f_;
  f2 = f1;
  f13 = c1 ? 1.0 : 3.0;
  f26 = f13 + f13;

/*@ assert f26 >= -1.0 ; */

  ca1 = f_0;
  ca2 = f13;
  ca3 = f0;
  ca4 = f00;

  m_ = - m0;
  m_0 = c1 ? m0 : m_;
  m00 = - m_;
  m2 = m1;
  m13 = c1 ? 1.0 : 3.0;
  m26 = m13 + m13;

  if (f2 == f1)
    d2 = d1;
  f3 = f1 + f0;
  if (f3 == f1)
    d6 = d1;

  f13 = c1 ? 1.0 : 3.0;

  A = f0 == f_;
  B = f0 == f1;
  C = f0 == f0;
  D = f_ == f1;
  E = f_ == f_;

  F = f_0 == f0;
  G = f_0 == f_;
  H = (c1 ? f0 : 3.0) == f_;
  I = (c1 ? f0 : 3.0) == f0;
  J = f13 == f_;
  K = f13 == f0;
  L = f13 == (c2? 3.0 : 5.0);

  P = f13 != (c2? 3.0 : 5.0);
  Q = f0 != f_;
  R = f0 != f1;

  Am = m0 == m_;
  Bm = m0 == m1;
  Cm = m0 == m0;
  Dm = m_ == m1;
  Em = m_ == m_;

  Fm = m_0 == m0;
  Gm = m_0 == m_;
  Hm = (c1 ? m0 : 3.0) == m_;
  Im = (c1 ? m0 : 3.0) == m0;
  Jm = m13 == m_;
  Km = m13 == m0;
  Lm = m13 == (c2? 3.0 : 5.0);

  t1 = f_0 <= f0;
  t2 = f0 <= f_0;
  t3 = f0 <= f13;
  t4 = f13 <= f26;
  t5 = f26 <= f13;
  t6 = 1.0 <= f26;
  t7 = f26 <= 1.0;
  t8 = f1 <= f1;

  s1 = f_0 < f0;
  s2 = f0 < f_0;
  s3 = f0 < f13;
  s4 = f13 < f26;
  s5 = f26 < f13;
  s6 = 1.0 < f26;
  s7 = f26 < 1.0;
  s8 = f1 < f1;

  d3 = d1 + 2.0;
  d4 = d1 + 2;

  if (1.0)  if1 = 1;
  if (0.0)  if2 = 1;
  if (-0.0) if3 = 1; 

  if (1.0)  ite1 = 1; else ite1 = 2;
  if (0.0)  ite2 = 1; else ite2 = 2;
  if (-0.0) ite3 = 1; else ite3 = 2;

  fic0 = C0;
  fic1 = 1;
  fic2 = C2;
  fic4 = C2 + C2;
  fec0 = (float) C0;
  fec2 = (float) C2;
  fec4 = (float) (C2 + C2);

  d5 = (c2 ? -3.0 : 9.0) / f13; 
  d7 = (c2 ? -3.0 : 9.0) / (-f13); 
}
