typedef enum E { C0, C1, C2 };

void f(enum E const);

void f1(enum E);

void g() { f((enum E) C0); f1((enum E)C2); }

void h() { f(C1); f1(C0); }
