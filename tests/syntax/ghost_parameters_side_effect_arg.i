void function(int x) /*@ ghost(int y) */ ;
int other(int x) /*@ ghost(int y) */ ;

void caller(){
  int x = 0 ;
  //@ ghost int g = 0 ;
  int t[] = { 0, 0, 0 } ;

  function(x++) /*@ ghost(g++) */ ;
  function(x = 2) /*@ ghost(g = 42) */ ;
  function(x += 2) /*@ ghost(g += 42) */ ;
  function(-x) /*@ ghost(-g) */ ;
  function( (x == 0) ? x : 42 ) /*@ ghost( (g == 0) ? g : 42 ) */ ;
  function(t[x++]) /*@ ghost(t[g++]) */ ;
  function( other(x) /*@ ghost(g) */ ) /*@ ghost( other(x, g) ) */ ;

  /*@ ghost
    int i = 1 ;
    function(g++, i++) ;
  */
}
