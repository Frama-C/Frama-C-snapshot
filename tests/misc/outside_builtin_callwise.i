/* run.config
   DONTRUN: OPT: -inout-callwise -val-builtin strlen:Frama_C_strlen -val
*/

int strlen ()
{
  return 0;
}

int main ()
{
  strlen(); // note that we pass the wrong number of arguments,
  // causing Db.Value.Outside_builtin_possibilities to be raised.
}
