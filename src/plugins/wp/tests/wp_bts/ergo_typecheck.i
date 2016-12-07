typedef struct
{
    unsigned int a[2];
    unsigned int b[2];
    unsigned int c;
} my_type;

my_type  var = {0};

/*@
  @ ensures  var_divded : var == {\old(var) \with
  @                                 .a = {\old(var.a) \with
  @                                     [0] = (unsigned int) 0,
  @                                     [1] = (unsigned int) 1},
  @                                 .b = {\old(var.b) \with
  @                                     [0] = (unsigned int)(\old(var.b[0]) + 1),
  @                                     [1] = (unsigned int)(\old(var.b[1]) + 2)},
  @                                 .c = (unsigned int) 5
  @                               };
  @
  @ ensures  var_inline : var == {\old(var) \with
  @                                 .a[0] = (unsigned int) 0,
  @                                 .a[1] = (unsigned int) 1,
  @                                 .b[0] = (unsigned int)(\old(var.b[0]) + 1),
  @                                 .b[1] = (unsigned int)(\old(var.b[1]) + 2),
  @                                 .c = (unsigned int) 5
  @                               };
  @
  @ ensures var_unit0 : var.a[0] == 0;
  @ ensures var_unit1 : var.a[1] == 1;
  @ ensures var_unit2 : var.b[0] == (unsigned int)(\old(var.b[0]) + 1);
  @ ensures var_unit3 : var.b[1] == (unsigned int)(\old(var.b[1]) + 2);
  @ ensures var_unit4 : var.c == 5;
  @ assigns var;
  @
 */
void f()
{
    var.a[0] = 0u;
    var.a[1] = 1u;
    var.b[0] ++;
    var.b[1] += 2;
    var.c = 5u;
}
