/* see bug #128 */

int e;

/*@ requires e > 0; */
void f(void);

// this spec is already lost after type-checking
/*@ requires e > 0; */
void g(void);

int x;

/*@ requires a > 0; */
void i(int a);

void i(int b) { x = b; }

void h() { f(); g();}
