/* run.config
   OPT: -wp-prop none -wp-log refusage
*/

/* run.config_qualif
   DONTRUN:
*/

int x0, x1, x2, x3, x4, x5, x6, x7, x8, x9;
int y0, y1, y2, y3, y4, y5, y6, y7, y8, y9;
int z0, z1, z2, z3, z4, z5, z6, z7, z8, z9;
int*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8,*p9;

struct st { int c; int * pc; int tc[5]; } s0;

int tab[], tab1[1], tab2[2];
char *ptr;

//-------------------------------------------------------
int no_access0;
void no_access_undef(int noaccess1);
void no_access(int noaccess2) { int noaccess3; }
//-------------------------------------------------------
void by_value_in_code(int x) {
  ptr=(char *) 0;
  if (y0)
    x0 = x ;
  while (!-(y1 - y2 + y3 * y4 / y5 & y6 | y7 ^ y8 % y9))
    x0 = !-(x1 - x2 + x3 * x4 / x5 & x6 | x7 ^ x8 % x9);
  switch (z0) {
    int z=0 ;
  case 1:
    z1=1;
    break;
  }
  tab[1+z2]=0;
}

void by_reference_in_code(int *p, int **qq) {
  *p=(int *) 0;
  **qq=*p1;

}

void by_addr_in_code(int v1, struct st s2, struct st s3) {
  if ((&x0 != &s0.c) && (&tab[5] != &v1) && (&s2.c != &s3.tc[3]))
    return;
}

void by_array_in_code(int *p, int *q, int **qq) {
  *(ptr+1)=0;
  *(p+1)=q[0];
  p1[p2[0]]=*(p3+p4[*(p5+1)]);
}
//-------------------------------------------------------
void by_value_in_code_annotation(int v, int *p, int*q) {
  //@ assert tab[v-1]==0 && \initialized (&x6);
  /*@ requires (x1?x2:x3)== 0;
    @ assigns x4;
    @*/
  /*@ loop invariant x5<0;
    @ loop variant q-p;
    @*/
  while (1)
    //@ assert 0 == \let term=1+\at(x7,Pre) ; 1+term;
    //@ assert \let pred=(x8==0) ; pred && x9==0;
    ;
}

//@ behavior no_exit: exits \false;
void by_reference_in_code_annotation(int*p) {
  //@ for no_exit: assert \valid (p);
  //@ ensures \separated (p1,p2) && \freeable (p3) || \allocable (p5) <==> \initialized (p6);
  ;
  //@ loop invariant *p4<0;
  while (1);
}

void by_addr_in_code_annotation(void) {
  //@ requires (&x0 != &s0.c) && &tab[5];
  return;
}

void by_array_in_code_annotation(int *p, int *q, int **qq) {
  //@ assert *(ptr+1)==0 && *(p+1)==q[0] && p1[p2[0]]==*(p3+p4[*(p5+1)]);
  ;
}
//-------------------------------------------------------
/*@ assigns x0, p0;
  @ ensures p1+x1==p2;
  @*/
void by_value_in_spec (void);

/*@ requires (\valid (p0) && \separated (p1,p2) ? \freeable (p3) || \allocable (p5) : \initialized (p6));
    ensures *p4==0;
    assigns *p;
*/
void by_reference_in_spec (int * p);

//@ requires (&x0 != &s0.c) && &tab[5];
void by_addr_in_spec (void);

//@ behavior blabla: ensures *(ptr+1)==0 && *(p+1)==q[0] && p1[p2[0]]==*(p3+p4[*(p5+1)]);
void by_array_in_spec (int *p, int *q, int **qq) ;
//-------------------------------------------------------

int val, *ref, *ref_bis, addr, *array, val_for_tab[], not_imported;
/*@ requires by_value: !val && val_for_tab[1];
  @ requires by_ref: \valid (ref) && !*ref_bis;
  @ requires by_addr: &addr!=(int*)0;
  @ assigns  by_array: *(array+1);
  @*/
void global_spec (void) {
  not_imported = 0;
}

/*@ requires by_value: !val_arg1 && !val_arg2 && !val_arg3;
  @ requires by_ref:   \separated (ref_arg1, ref_arg2, ref_arg3);
  @ requires by_addr: &addr_arg0.tc[5] != (char*) 0;
  @ assigns  by_array: array_arg1[val_arg0], array_arg2[2], array_arg3[3];
  @*/
void params_spec (int val_arg0, struct st addr_arg0,
		  int **val_arg1, int *val_arg2, int *val_arg3,
		  int **ref_arg1, int *ref_arg2, int *ref_arg3,
                  int *array_arg1,int *array_arg2,int *array_arg3) {
  not_imported = 0;
}

int val1;
//@ requires val1==v;
void global_and_param_spec (int v) {
  not_imported = 0;
}

// Some global variables
int val0;
int *addr_val1, *val_val2, *val_val3;
int *val_ref1, *ref_ref2, *array_ref3;

void calling_spec (int v1, int v2, int v3, int v4) {
  // Some local variables
  struct st no_access_addr0;
  struct { int a; int b ;} val_array1;
  int *array_array2, *array_array3;
  global_spec();
  params_spec (val0, no_access_addr0,
	       &addr_val1, val_val2, val_val3+v1,
	       &val_ref1, ref_ref2, array_ref3+v2,
	       &val_array1.a, array_array2, array_array3+v3);
  global_and_param_spec (v4);
}

int addr_value;
/*@ requires by_value: !val && !val_ref && !value_array && !addr_value;
    requires by_ref:   \separated (ref, val_ref, array_ref) ;
    requires by_addr:  &addr != &addr_value;
  @ assigns  by_array: *(array+1), *(array_ref+1), *(value_array+1);
*/
void cup (int *val_ref, int *array_ref, int *value_array) ;
