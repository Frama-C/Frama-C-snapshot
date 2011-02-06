//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//------ example from BTS #312 
/*@ exits never_exits: oracle_ok: \false;
    behavior oracle_ko :
      ensures oracle_ko: \false; // <- invalid property
*/
int main() {
  return 1;
}

/*@ assigns \nothing ;
    ensures never_returns: \false;
    exits   exit_status:   \exit_status==status;
 */
extern void exit(int status);

int X ;
/*@ behavior never_exits:
      assumes x>=0;
      exits   never_returns: oracle_ok: \false;
    behavior never_returns:
      assumes x<0;
      assigns \nothing ;
      ensures never_returns: oracle_ok: \false;
      exits   exit_status:   oracle_ok: \exit_status==1;
    behavior oracle_ko :
      assumes x<0;
      exits   oracle_ko: \false; // <- invalid property
*/
int may_exit(int x) {
  if (x < 0)
    exit(1);
  X = 1;
  return 0 ;
}

/*@ behavior never_returns :
      assumes x<0;
      assigns oracle_ok: X;
    behavior all :
      assigns oracle_ok: X;
      ensures result_value: oracle_ok: \result == 0;
      exits   exit_status:  oracle_ok: \exit_status==1;
    behavior oracle_ko:
      assumes x<0;
      assigns \nothing ; // <- invalid property
    behavior oracle_bis_ko :
      ensures oracle_ko: \false; // <- invalid property
      exits   oracle_ko: \false; // <- invalid property
*/
int may_exit_bis(int x) {
  X = 1;
  if (x < 0)
    exit(1);
  return 0 ;
}

/*@ assigns \nothing ;
  @ ensures never_returns: \false ;
  @ exits   exit_status:   \exit_status == status ;
  @ */
void exit (int status);
int status ;

/*@ assigns oracle_ok: status ;
  @ exits   oracle_ok: ! cond && \exit_status == 1 && status == val ;
  @ */
void may_exit2 ( int cond , int val ) {
    if (! cond ) {
       status = val ;
       exit (1);
    }
}

int stmt_never_exit (int c) {
  int x;

  /*@behavior ok: exits oracle_ok: 1 == 2; */
  if (c) x = 1;
  else x = 0;

  return x;
}

int stmt_may_exit (int c) {
  int x;

  /*@behavior ok: exits oracle_ok: c && \exit_status == 1; */
  if (c) exit (1);
  else x = 0;
  return x;
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

