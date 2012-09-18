// GCC allows such things

int f(int);

int f(x) short x; { return x; }

// but not that

int g(int);

int g(short x) { return x; }
