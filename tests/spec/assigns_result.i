/* run.config
   STDOPT: +"-deps @EVA_OPTIONS@"
*/
int X,Y;

/*@ assigns \result;
    assigns \exit_status;
*/
int f(void);

/*@ assigns \result \from X;
    assigns \exit_status \from Y;
*/
int g(void);

void main(void) { f(); g(); }
