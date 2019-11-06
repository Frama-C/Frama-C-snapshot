void decl_function_void_no_ghost(void);
void def_function_void_no_ghost(void) {}
void decl_function_void_ghost(void) /*@ ghost (int y) */;
void def_function_void_ghost(void) /*@ ghost (int y) */ {}
void decl_function_x_no_ghost(int x);
void def_function_x_no_ghost(int x) {}
void decl_function_x_ghost(int x) /*@ ghost (int y) */;
void def_function_x_ghost(int x) /*@ ghost (int y) */ {}
void decl_with_fptr(void (*ptr)(int x) /*@ ghost (int y) */);
void def_with_fptr(void (*ptr)(int x) /*@ ghost (int y) */) {
  void (*local)(int) /*@ ghost (int) */ = ptr;

  (*local)(4) /*@ ghost(2) */;
  //@ ghost (*local) (4, 2) ;
}
void decl_variadic(int x, ...) /*@ ghost(int y) */;
void def_variadic(int x, ...) /*@ ghost(int y) */ {}

int main(void) {
  decl_function_void_no_ghost();
  def_function_void_no_ghost();
  decl_function_void_ghost() /*@ ghost (4) */;
  def_function_void_ghost() /*@ ghost (4) */;
  decl_function_x_no_ghost(2);
  def_function_x_no_ghost(2);
  decl_function_x_ghost(2) /*@ ghost (4) */;
  def_function_x_ghost(2) /*@ ghost (4) */;
  decl_with_fptr(&decl_function_x_ghost);
  def_with_fptr(&decl_function_x_ghost);
  decl_variadic(2, 1, 2, 3, 4) /*@ ghost(4) */;
  def_variadic(2, 1, 2, 3, 4) /*@ ghost(4) */;

  /*@ ghost
    decl_function_void_no_ghost();
    def_function_void_no_ghost();
    decl_function_void_ghost(4);
    def_function_void_ghost(4);
    decl_function_x_no_ghost(2);
    def_function_x_no_ghost(2);
    decl_function_x_ghost(2,4);
    def_function_x_ghost(2,4);
    decl_with_fptr(&decl_function_x_ghost);
    def_with_fptr(&decl_function_x_ghost);
    decl_variadic(2, 1, 2, 3, 4, 4);
    def_variadic(2, 1, 2, 3, 4, 4);
  */
}