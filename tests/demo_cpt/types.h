#include <stdlib.h>

#define INCR_OP_NB 100

typedef enum { false = 0, true } bool;

typedef struct {
  char day;
  char month;
  int year;
} t_date;

typedef struct {
  t_date date;
  char * description;
  int bank_code; // 0 iff not a bank operation
  int amount;
} t_operation;

const int op_size = sizeof(t_operation);

typedef struct {
  int size; // taille du tableau d'operations
  int nb; // nombre d'operation dans le tableau
  t_operation * ops;
} t_operations;


/*@ predicate operations_ok{L}(t_operations ops) =
    0 <= ops.nb && ops.nb <= ops.size && \valid(ops.ops);
*/

typedef struct {
  char * name;
  t_operations operations;
  int balance;
  int bk_balance;
} t_account;
/*@ predicate account_ok{L}(t_account c) =
    operations_ok{L}(c.operations);
*/

extern t_account BankAcc;
extern t_account SaveAcc;

extern t_operations BankOps;

//@ ensures \result == false || account_ok{Here}(*c);
bool init_account (char * filename, t_account * c);

//@ assigns \nothing;
bool save_account (char * filename, t_account * c);

/*@ assigns BankOps \from \nothing;
    ensures 0 <= BankOps.nb;
    ensures operations_ok{Here}(BankOps);
*/
bool get_bank_operations (void);

