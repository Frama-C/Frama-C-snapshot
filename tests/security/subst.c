/* run.config
   GCC:
   OPT: -security-analysis -security-lattice weak
*/

/*@ requires security_status(y) == public; */
void send(const int y);


int Z;
/*@ ensures security_status(Z) == public; */
void crypt_Z();


/*@ ensures security_status( *z) == public; */
void crypt(int* z);

/*@ requires security_status( *t) == public; */

void f(int* t) { crypt(t); }

int x;
int y;
int z;

void main() {
  f(&y);
  send(y);
  send(x);
  x = 1;
  f(&x);
  f(&z);
  crypt(&x);
  send(x);
}
