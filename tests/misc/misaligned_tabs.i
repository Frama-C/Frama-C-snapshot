char T[300];

struct st { short i1,i2; char c1,c2; short i3,i4 ;};
struct st S1 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;
struct st S2 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;
struct st S3 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;
struct st S4 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;

void misaligned_struct() {
  Frama_C_show_each_1 (S1.i1 == *(short*)&S1.c1); // ok
  
  *(  (char *)&S1.i1)= 0x11;
  Frama_C_show_each_2 (S1.i1 == 0x1111); // To do
  Frama_C_show_each_3 (S1.i1 == S1.i2);  // To do
  Frama_C_show_each_4 (*(char *)&S1.i1 == S1.c2); // OK
  
  *(  (char *)&S2.i1)= 0x11;
  *(1+(char *)&S2.i1)= 0x11;
  Frama_C_show_each_5 (S2.i1 == 0x1111); // ok
  Frama_C_show_each_6 (S2.i1 == S2.i2);  // ok
  Frama_C_show_each_7 (*(char *)&S2.i2 == S2.c2); // OK
  Frama_C_show_each_8 (*(char *)&S2.i2 == *(char *)&S4.i2); // OK
 
  *(1+(char *)&S3.i1)= 0x11;
  *(  (char *)&S3.i2)= 0x11;
  *(1+(char *)&S3.i2)= 0x11;
  *(  (char *)&S3.i3)= 0x11;
  *(1+(char *)&S3.i3)= 0x11;
  *(  (char *)&S3.i4)= 0x11;

  *(  (char *)&S4.i1)= 0x11;
  *(1+(char *)&S4.i1)= 0x11;
  *(  (char *)&S4.i2)= 0x11;
  *(1+(char *)&S4.i2)= 0x11;
  *(  (int  *)&S4.c1)= 0x1111;
  *(  (char *)&S4.i3)= 0x11;
  *(1+(char *)&S4.i3)= 0x11;
  *(  (char *)&S4.i4)= 0x11;
  *(1+(char *)&S4.i4)= 0x11;

  Frama_C_show_each_9 (S3.i1 == S4.i1);                          // To do
  Frama_C_show_each_a (S3.i2 == S4.i2);                          // ok
  Frama_C_show_each_b (S3.i3 == S4.i3);                          // ok
  Frama_C_show_each_c (*((char *)&S3.i2) == *((char *)&S4.i2)); // OK
  Frama_C_show_each_d (S3.c1 == S4.c2);             // OK
  Frama_C_show_each_e (*((char *)&S3.i2) == S4.c1); // Ok
  Frama_C_show_each_f (*((char *)&S3.i1) == S4.c1); // Ok
}

void main(int c1, int c2) {

  
  *(int*)(&T[0])=c1?1:2;
  *(int*)(&T[4])=c2?1:2;
  T[1]=T[5];
  *(int*)(&T[8])=*(int*)(&T[4]);

  misaligned_struct ();
/*  for(i = 0; i < 36800; i++) {
    T[i] = 33;
    }
*/

  if (c1) Frama_C_show_each_g (S1.i1 == *(short*)&S1.c1); // to do

}
