//@ exits never_exits: \false;
int main() { return 0; }

/*@ assigns \nothing ;
    ensures never_returns: \false;
    exits \exit_status==status;
 */
extern void exit(int status);

/*@ behavior never_exits:
      assumes x>=0;
      exits \false;
    behavior never_returns:
      assumes x<0;
      assigns \nothing ;
      ensures never_returns: \false;
      exits \exit_status==x;
*/
int may_exit(int x) { if (x) exit(0); return 0; }

// Following spec must be rejected

//@ exits \result == 0;
int f () { return 0; }

//@ requires \exit_status == 0; ensures \exit_status == 0;
void g () { 
  //@ assert \exit_status == 0; 
  exit(0); 
}
