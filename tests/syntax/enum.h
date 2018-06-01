enum e {E1, E2};

#ifndef V
enum f {F11, F12};
enum {K11, K12};
enum {U1,U2}; // unused, but changes the anonymous enum counter between the two files involved in the test
#else
enum f {F21, F22};
enum   {K21, K22};
#endif

enum { I1, I2};
