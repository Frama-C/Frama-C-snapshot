int i1 = 10; /* Definition, external linkage */
static int i2 = 20; /* Definition, internal linkage */
extern int i3 = 30; /* Definition, external linkage */
int i4; /* Tentative definition, external linkage */
 static int i5; /* Tentative definition, internal linkage */
int i1; /* Valid tentative definition */
int i2; /* Undefined, linkage disagreement with previous */
int i3; /* Valid tentative definition */
int i4; /* Valid tentative definition */
int i5; /* Undefined, linkage disagreement with previous */
int main(void) {
/* ... */
return i1 + i2 + i3 + i4 + i5;
}
