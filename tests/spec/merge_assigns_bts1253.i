/*@ assigns \result \from nptr[..] ; */
double atof(const char *nptr);
 
/*@ assigns \result \from *(nptr+(..)); */
double atof(const char *nptr);

/*@ assigns nptr[..] ; */
void f(char *nptr);
 
/*@ assigns *(nptr+(..)); */
void f(char *nptr);

