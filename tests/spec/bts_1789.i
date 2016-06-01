/* run.config_qualif
   DONTRUN:
*/

int *var_ptr;

//@ logic int *the_var = var_ptr;
//@ assigns the_var;
void function(){
  var_ptr = 0L;
}

