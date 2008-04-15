
#define false 0
#define true 1
#ifndef NULL
#define NULL 0
#endif /* NULL */

struct Proc {
  struct Proc *next;
  int priority;
  int pid;
};

int max_prio;

//@ ensures \valid(\result);
struct Proc *pid2pcb(unsigned int);

struct Proc *ready_list;

/*@ axiomatic ListLength {
  @  logic int list_length{State}(struct Proc *p);
  @      // reads p->next, p->next->next;
  @
  @  axiom list_length_zero{State}:
  @   \forall struct Proc *p; list_length(p) == 0 <==> p == NULL;
  @
  @  axiom list_length_pos{State}:
  @   \forall struct Proc *p; list_length(p) > 0 ==> 
  @     (\valid(p) && list_length(p->next) == list_length(p) - 1);
  @
  @  axiom list_length_remove{State}:
  @   \forall struct Proc *p; list_length(p) > 0 ==> 
  @     (\valid(p) && list_length(p->next) == list_length(p) - 1);
  @ }
  @*/

/*@ axiomatic Reachable {
  @   predicate reachable{State}(struct Proc *p, struct Proc *q);
  @        // reads p->next;
  @   axiom reachable_def{State}:
  @    \forall struct Proc *p, *q; reachable(p,q) ==> 
  @     p == q || reachable(p->next,q);
  @ }
  @*/

/* axiom 
  @   \forall struct Proc *p, *q; reachable(p,q) ==> 
  @     p == q || 
  @*/

/*@ requires list_length(ready_list) >= 0;
  @*/
int process_kill(unsigned int pid) {
  int found = false;
  unsigned proc_id = pid & 127u;
  unsigned proc_id2;
  struct Proc *process = pid2pcb(proc_id);
  struct Proc *prev_elem = NULL;
  struct Proc *ready_list_elem = ready_list;
  struct Proc *highest_search = NULL;
  int highest_prio;
  /*@ loop invariant list_length(ready_list) >= 0
    @             && list_length(ready_list_elem) >= 0
    @             && list_length(prev_elem) >= 0
    @             && list_length(highest_search) >= 0;
    @*/
  while ((ready_list_elem != NULL) && (found == false)) {
    proc_id2 = ready_list_elem->pid;
    if (proc_id == proc_id2) {
      if (prev_elem != NULL)
	prev_elem->next = ready_list_elem->next;
      else
	ready_list = ready_list_elem->next;
      ready_list_elem->next = NULL;
      if (process->priority == max_prio) {
	highest_prio = 0u;
	highest_search = ready_list;
	/*@ loop invariant list_length(highest_search) >= 0;
	  @*/
	while (highest_search != NULL) {
	  if (highest_search->priority > highest_prio)
	    highest_prio = highest_search->priority;
	  highest_search = highest_search->next;
	}
	max_prio = highest_prio;
      }
      found = true;
    }
    prev_elem = ready_list_elem;
    ready_list_elem = ready_list_elem->next;
  }
  return 0;
}

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make vamos"
End:
*/
