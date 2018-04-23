int T[30] = {1};

int   f_int_int(int x);

extern int g; int *pg = &g;
/*@ assigns \result \from pg; */
int * f_int_star_int(int x);

int **ppg = &pg;

/*@ assigns \result \from ppg; */
int **f_int_star_int_star_int(int x);

int f_star_int_cint(const int *x);

/* 3 identicals prototypes */
int f_star_int_int(int *x);
int f_tab_int_int(int x[]);
int f_tab3_int_int(int x[3]);

int cv1=10, cv2=20, cv3=30 ;
struct _st_star_cint { const int * p ; }
  st_star_cint_1={&cv1},
  st_star_cint_2={&cv2},
  st_star_cint_3={&cv3} ;

int v1=10, v2=20, v3=30 ;
struct _st_star_int { int * p ; }
  st_star_int_1={&v1},
  st_star_int_2={&v2},
  st_star_int_3={&v3} ;

struct _st_tab3_int { int t[3] ; }
  st_tab3_int_1={10, 11, 12},
  st_tab3_int_2={20, 21, 22},
  st_tab3_int_3={30, 31, 32} ;

struct _st_star_cint f_st_star_cint_st_star_cint(struct _st_star_cint s) ;
struct _st_star_int  f_st_star_int_st_star_int (struct _st_star_int s) ;
struct _st_tab3_int  f_st_tab3_int_st_tab3_int (struct _st_tab3_int s) ;

int f_star_st_star_cint_int (struct _st_star_cint * s) ;
int f_star_st_star_int_int (struct _st_star_int * s) ;
int f_star_st_tab3_int_int (struct _st_tab3_int * s) ;

void main() {
  int c,d;
  T[0]=f_int_int(0); /* T[0] modified */
  
  int *p = f_int_star_int(0);
  Frama_C_show_each_F(*p);
  *p = 5;
  Frama_C_show_each_F(*p);

  int **pp  =f_int_star_int_star_int(0);
  Frama_C_show_each_G(*pp);
  Frama_C_show_each_F(**pp);
//  if (*pp==&d) **pp = 6;
  Frama_C_show_each_G(*pp);
  Frama_C_show_each_F(**pp);

  T[2]=f_star_int_cint(&T[3]); /* T[2] modified */

  f_star_int_int(&(T[4])); /* only T[4] modified */
  f_tab3_int_int(&T[6]);   /* only T[6..8] modified */
  f_tab_int_int(&T[10]);   /* only T[10] modified */

  st_star_cint_1 = f_st_star_cint_st_star_cint(st_star_cint_2); /* only st_star_cint_1 modified */
  st_star_int_1  = f_st_star_int_st_star_int (st_star_int_2) ;  /* st_star_int_1 modifed, v2 SHOULD BE modified */
  st_tab3_int_1  = f_st_tab3_int_st_tab3_int (st_tab3_int_2) ;  /* only st_tab3_int_1 modified */
  
  f_star_st_star_cint_int(&st_star_cint_3); /* st_star_cint_3.p modified */
  f_star_st_star_int_int (&st_star_int_3) ; /* v3 SHOULD BE modified */
  f_star_st_tab3_int_int (&st_tab3_int_3) ; /* st_tab3_int_3 SHOULD BE modified */

}

