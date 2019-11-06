/* run.config
  MODULE: @PTEST_DIR@/@PTEST_NAME@.cmxs
  OPT: -print
*/

void nothing(void);
int nothing_r(void);

void something_non_ghost(int *p);
void something_ghost(void) /*@ ghost (int* p) */;

int something_non_ghost_r(int *p);
int something_ghost_r(void) /*@ ghost (int* p) */;

void both(int *p, int x) /*@ ghost (int* gp, int gx) */;
int both_r(int *p, int x) /*@ ghost (int* gp, int gx) */;

/*@ ghost
  void g_nothing(void);
  int g_nothing_r(void);
  void g_something_non_ghost(int *p);
  int g_something_non_ghost_r(int *p);
  void g_both(int *p, int x, int *gp, int gx);
  int g_both_r(int *p, int x, int *gp, int gx);
*/

void reference(void) {
  nothing();
  nothing_r();
  something_non_ghost(0);
  something_ghost() /*@ ghost (0) */;
  something_non_ghost_r(0);
  something_ghost_r() /*@ ghost (0) */;
  both(0, 1) /*@ ghost (0, 2) */;
  both_r(0, 1) /*@ ghost (0, 2) */;

  /*@ ghost
    g_nothing();
    g_nothing_r();
    g_something_non_ghost(0);
    g_something_non_ghost_r(0);
    g_both(0, 1, 0, 2);
    g_both_r(0, 1, 0, 2);
  */
}