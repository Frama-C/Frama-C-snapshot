/* run.config
   DONTRUN: don't run a test which raises an exception
*/
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//------ example from BTS #312 
//@ exits never_exits: \false;
int main() {
  return 1;
}

/*@ assigns \nothing ;
    ensures never_returns: \false;
    exits \exit_status==status;
 */
extern void exit(int status);

int X ;
/*@ behavior never_exits:
      assumes x>=0;
      exits \false;
    behavior never_returns:
      assumes x<0;
      assigns \nothing ;
      ensures never_returns: \false;
      exits \exit_status==1;
*/
int may_exit(int x) {
  if (x < 0)
    exit(1);
  X = 1;
  return 0 ;
}

/*@ behavior never_returns_ko:
      assumes x<0;
      assigns \nothing ; // <- invalid property
    behavior never_returns :
      assumes x<0;
      assigns X;
    behavior all :
      assigns X;
      ensures \result == 0;
      exits \exit_status==1;
*/
int may_exit_bis(int x) {
  X = 1;
  if (x < 0)
    exit(1);
  return 0 ;
}

/*@ assigns \nothing ;
  @ ensures \false ;
  @ exits   \exit_status == status ;
  @ */
void exit (int status);
int status ;

/*@ assigns status ;
  @ exits   ! cond && \exit_status == 1 && status == val ;
  @ */
void may_exit2 ( int cond , int val ) {
    if (! cond ) {
       status = val ;
       exit (1);
    }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

