/* frama-c-gui -val -deps account.c */
/* frama-c-gui -val -deps account.c -main update_account -slice-pragma update_account */
/* frama-c-gui -val -deps account.c -main update_account -sparecode-analysis */
/* frama-c -jessie -jessie-gui inv.c -jessie-int-model exact account.c
 */

#include "types.h"

bool increase_size (t_operations * ops) {
  int i;
  int new_size = ops->size + INCR_OP_NB;
  t_operation * new_ops = (t_operation *) malloc (new_size * op_size);
  if (new_ops) {
    t_operation * old_ops = ops->ops;
    ops->ops = new_ops;
    ops->size = new_size;
    for (i = 0; i < ops->nb; i++) {
      ops->ops [i] = old_ops[i];
    }
    free (old_ops);
    return true;
  }
  return false;
}

/*@ requires account_ok(*c);
  @ ensures \result == false || account_ok(*c);
  */
bool add_operation_to_account (t_operation * op, t_account * c) {

  if (c->operations.nb == c->operations.size) {
    bool ok = increase_size (&(c->operations));
    if (!ok) return false;
  }

  c->operations.ops[c->operations.nb] = *op;
  c->operations.nb ++;

  c->balance += op->amount;
  if (op->bank_code != 0)
    c->bk_balance += op->amount;

  return true;
}

  
/*@ ensures min <= balance + \result <= max;
 */
int check_account (int balance, int min, int max) {
  int res = 0;
  if (balance < min)
    res = balance - min;
  if (balance > max)
    res = max - balance;
  return res;
}

/*@ requires \valid(bk_ops) && \valid(bk_ops->ops + (0..bk_ops->nb-1)); 
 */
bool process_bank_operations (t_operations * bk_ops, t_account * c) {
  int i; bool ok = true;
  for (i = 0; i < bk_ops->nb; i++) {
    ok = add_operation_to_account (bk_ops->ops + i, c);
    if (!ok) break;
  }
  return ok;
}

/*@ requires \valid(bk_ops) && \valid(bk_ops->ops + (0..bk_ops->nb-1)); 
 */
int update_account (t_operations * bk_ops, t_account * c) {
  int todo;
  int min = 200;
  int max = 2000;
  char * fname = "current.cpt";

  process_bank_operations (bk_ops, c);

  todo = check_account (c->balance, min, max);
  /*
  if (todo < 0) {
    // get money from savings... if any !
    t_operation op = { today(), "From savings", 0, x };
 */

  //@ slice pragma expr c->balance;

  return 0;
}
