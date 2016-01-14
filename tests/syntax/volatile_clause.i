typedef volatile unsigned Vunsigned;

unsigned g(Vunsigned * q);
unsigned f(volatile unsigned * q);

volatile unsigned * p = (volatile unsigned *)(0x4);

//@ volatile *p reads g;
//@ volatile *((Vunsigned *)(0x4)) reads f;
//@ volatile *((unsigned volatile *)(0x6)) reads f;

