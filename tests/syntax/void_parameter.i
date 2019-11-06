/* run.config

*/

// GCC warning, Clang/CompCert error
void f1(void);
void f2(void parameter);
void f3(void, int x);
void f4(void parameter, int x);

// GCC/Clang/CompCert error
void f1(void){}
void f2(void parameter){}
void f3(void, int x){}
void f4(void parameter, int x){}

