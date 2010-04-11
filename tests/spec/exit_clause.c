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
