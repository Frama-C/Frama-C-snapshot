typedef volatile int VINT;

extern int f(int);

inline int r(VINT* v) { return *v; }

inline int w(volatile int* v, int new) { *v = new; return new; }

volatile int v, tab[10];
VINT *pt;

struct st { int a ; volatile int v ; } s ; 
//@ volatile v, tab[..] reads r writes w;
//@ volatile *pt writes w;
//@ volatile s.v reads r;

typedef struct st ST ;
struct vst { int b ; ST v ; } vs ; 
// some parts of vs have volatile qualifier
struct vst rs (struct vst * p) ;
struct vst ws (struct vst * p, struct vst v) ;
//@volatile vs reads rs writes ws ;

volatile int x ;

volatile int y ;

volatile int z;
