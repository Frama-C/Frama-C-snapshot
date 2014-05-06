/* run.config
STDOPT: +"-enums int"
STDOPT: +""
*/

enum e { E0 = 0, E1 } X;
//@ ensures P: X == E0 && x == E1 && X < x && E0 <= E1 ==> \result == E0;
enum e f(enum e x) { X = E0; return E0; }

//@ ensures P: \result == ((unsigned) E0);
enum e g(enum e x) { return (unsigned) E0 ; }


signed   int s;
unsigned int u;
signed   long long hs;
unsigned long long hu;

enum e_v1 { V1=0 } v1, vv1;  // underlying type = unsigned int
enum e_v2 { V2=0 } v2;
enum e_u1 { U1=0xFFFFFFFFU } u1, uu1; // underlying type = unsigned int
enum e_u2 { U2=0xFFFFFFFFU } u2;
enum e_s1 { S1=-1 } s1, ss1; // underlying type = signed int
enum e_s2 { S2=-1 } s2;
enum e_h1 { H1=0xFFFFFFFFFFFFFFFFULL } h1; // underlying = unsigned long long

void us() {// Oracles without -enums option -> have to be confirmed
  u = s;  //  u = (unsigned int)s;
  s = u;  //  s = (int) u;
  u = U1; //- u = U1;
  s = U1; //  s = (int) U1;
  u = S1; //  u = (unsigned int)S1;
  s = S1; //  s = S1;
  u = V1; //- u = V1;
  s = V1; //  s = (int) V1;
  u = H1; //  u = (unsigned int)H1;
  s = H1; //  u = (int)H1;
}

void uu(){// Oracles without -enums option -> have to be confirmed
  uu1 = u;  //  uu1 = (enum e_u1) u;
  uu1 = u1; //  uu1 = u1;
  uu1 = U1; //- uu1 = U1;
  uu1 = u2; //  uu1 = (enum e_u1) u2;
  uu1 = U2; //  uu1 = (enum e_u1) U2;
  uu1 = s;  //- uu1 = (enum e_u1) s;
  uu1 = s1; //  uu1 = (enum e_u1) s1;
  uu1 = S1; //- uu1 = (enum e_u1) S1;
  uu1 = v1; //  uu1 = (enum e_u1) v1;
  uu1 = V1; //- uu1 = (enum e_u1) V1;
  uu1 = H1; //- uu1 = (enum e_u1) H1;
}

void ss(){// Oracles without -enums option -> have to be confirmed
  ss1 = u ; //  ss1 = (enum e_s1) u;
  ss1 = u1; //  ss1 = (enum e_s1) u1;
  ss1 = U1; //  ss1 = (enum e_s1) U1;
  ss1 = s ; //- ss1 = (enum e_s1) s;
  ss1 = s1; //  ss1 = s1;
  ss1 = S1; //  ss1 = S1;
  ss1 = s2; //  ss1 = (enum e_s1) s2;
  ss1 = S2; //- ss1 = (enum e_s1) S2;
  ss1 = v1; //  ss1 = (enum e_s1) v1;
  ss1 = V1; //- ss1 = (enum e_s1) V1;
}

void u_signed(){// Oracles without -enums option -> have to be confirmed
  u1 = (signed int) u ;  //- u1 = (enum e_u1) ((int) u);
  u1 = (signed int) u1 ; //- u1 = (enum e_u1) ((int) u1);
  u1 = (signed int) U1 ; //- u1 = (enum e_u1) ((int) U1);
  u1 = (signed int) u2 ; //- u1 = (enum e_u1) ((int) u2);
  u1 = (signed int) U2 ; //- u1 = (enum e_u1) ((int) U2);
  u1 = (signed int) s ;  //- u1 = (enum e_u1) s;
  u1 = (signed int) s1 ; //- u1 = (enum e_u1) ((int)) s1);
  u1 = (signed int) S1 ; //- u1 = (enum e_u1) S1;
  u1 = (signed int) s2 ; //- u1 = (enum e_u1) ((int)) s2)
  u1 = (signed int) S2 ; //- u1 = (enum e_u1) S2;
  u1 = (signed int) v1 ; //- u1 = (enum e_u1) ((int)) v1)
  u1 = (signed int) V1 ; //- u1 = (enum e_u1) ((int) V1);
}

void u_unsigned(){// Oracles without -enums option -> have to be confirmed
  u1 = (unsigned int) u  ; //  u1 = (enum e_u1) u;
  u1 = (unsigned int) u1 ; //  u1 = (enum e_u1) ((unsigned int) u1);
  u1 = (unsigned int) U1 ; //- u1 = U1;
  u1 = (unsigned int) u2 ; //  u1 = (enum e_u1) ((unsigned int) u2);
  u1 = (unsigned int) U2 ; //- u1 = (enum e_u1) U2;
  u1 = (unsigned int) s  ; //  u1 = (enum e_u1) ((unsigned int) s);
  u1 = (unsigned int) s1 ; //  u1 = (enum e_u1) ((unsigned int) s1);
  u1 = (unsigned int) S1 ; //  u1 = (enum e_u1) ((unsigned int) S1);
  u1 = (unsigned int) s2 ; //  u1 = (enum e_u1) ((unsigned int) s2);
  u1 = (unsigned int) S2 ; //  u1 = (enum e_u1) ((unsigned int) S2);
  u1 = (unsigned int) v1 ; //  u1 = (enum e_u1) ((unsigned int) v1);
  u1 = (unsigned int) V1 ; //  u1 = (enum e_u1) V1;
  u1 = (unsigned int) H1 ; //  u1 = (enum e_u1) ((unsigned int) H1);
}

void s_signed(){// Oracles without -enums option -> have to be confirmed
  s1 = (signed int) u  ; //- s1 = (enum e_s1) ((int) u);
  s1 = (signed int) u1 ; //- s1 = (enum e_s1) ((int) u1);
  s1 = (signed int) U1 ; //- s1 = (enum e_s1) ((int) U1);
  s1 = (signed int) u2 ; //- s1 = (enum e_s1) ((int) u2);
  s1 = (signed int) U2 ; //- s1 = (enum e_s1) ((int) U2);
  s1 = (signed int) s  ; //- s1 = (enum e_s1) s;
  s1 = (signed int) s1 ; //- s1 = (enum e_s1) ((int) s1);
  s1 = (signed int) S1 ; //  s1 = S1;
  s1 = (signed int) s2 ; //- s1 = (enum e_s1) ((int)s2);
  s1 = (signed int) S2 ; //- s1 = (enum e_s1) S2;
  s1 = (signed int) v1 ; //- s1 = (enum e_s1) ((int)v1);
  s1 = (signed int) V1 ; //- s1 = (enum e_s1) ((int) V1);
  s1 = (signed int) H1 ; //- s1 = (enum e_s1) ((int) H1);
}

void s_unsigned(){// Oracles without -enums option -> have to be confirmed
  s1 = (unsigned int) u  ; //  s1 = (enum e_s1) u;
  s1 = (unsigned int) u1 ; //  s1 = (enum e_s1) ((unsigned int) u1);
  s1 = (unsigned int) U1 ; //- s1 = (enum e_s1) U1;
  s1 = (unsigned int) u2 ; //  s1 = (enum e_s1) ((unsigned int) u2);
  s1 = (unsigned int) U2 ; //- s1 = (enum e_s1) U2;
  s1 = (unsigned int) s  ; //  s1 = (enum e_s1) ((unsigned int) s);
  s1 = (unsigned int) s1 ; //  s1 = (enum e_s1) ((unsigned int) s1);
  s1 = (unsigned int) S1 ; //  s1 = (enum e_s1) ((unsigned int) S1);
  s1 = (unsigned int) s2 ; //  s1 = (enum e_s1) ((unsigned int) s2);
  s1 = (unsigned int) S2 ; //  s1 = (enum e_s1) ((unsigned int) S2);
  s1 = (unsigned int) v1 ; //  s1 = (enum e_s1) ((unsigned int) v1);
  s1 = (unsigned int) V1 ; //  s1 = (enum e_s1) V1;
  s1 = (unsigned int) H1 ; //  s1 = (enum e_s1) ((unsigned int) H1);
}
