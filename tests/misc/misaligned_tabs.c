char T[300];

void CEA_assert (int);

struct st { short i1,i2; char c1,c2; short i3,i4 ;};
struct st S1 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;
struct st S2 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;
struct st S3 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;
struct st S4 = { 0x1111, 0x1111, 0x11,0x11, 0x1111, 0x1111} ;

misaligned_struct() {
  CEA_assert (S1.i1 == *(short*)&S1.c1); // To Do 
  
  *(  (char *)&S1.i1)= 0x11;
  CEA_assert (S1.i1 == 0x1111); // To do
  CEA_assert (S1.i1 == S1.i2);  // To do
  CEA_assert (*(char *)&S1.i1 == S1.c2); // OK
  
  *(  (char *)&S2.i1)= 0x11;
  *(1+(char *)&S2.i1)= 0x11;
  CEA_assert (S2.i1 == 0x1111); // To do
  CEA_assert (S2.i1 == S2.i2);  // To do
  CEA_assert (*(char *)&S2.i2 == S2.c2); // OK
  CEA_assert (*(char *)&S2.i2 == *(char *)&S4.i2); // OK
 
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

  CEA_assert (S3.i1 == S4.i1);                          // To do
  CEA_assert (S3.i2 == S4.i2);                          // To do
  CEA_assert (S3.i3 == S4.i3);                          // To do
  CEA_assert (*((char *)&S3.i2) == *((char *)&S4.i2)); // OK
  CEA_assert (S3.c1 == S4.c2);             // OK
  CEA_assert (*((char *)&S3.i2) == S4.c1); // Ok
  CEA_assert (*((char *)&S3.i1) == S4.c1); // Ok
}

int main(int c1, int c2) {
  int i;
  
  *(int*)(&T[0])=c1?1:2;
  *(int*)(&T[4])=c2?1:2;
  T[1]=T[5];
  *(int*)(&T[8])=*(int*)(&T[4]);

  misaligned_struct ();
/*  for(i = 0; i < 36800; i++) {
    T[i] = 33;
    }
*/

  if (c1) CEA_assert (S1.i1 == *(short*)&S1.c1);

  return i;
}
