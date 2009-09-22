/*@ type foo<a>; */

/* should be rejected (free type variable) */
/*@ logic integer bad(foo<a> x); */

/*@ logic integer f<a>(foo<a> x); */

/*@ logic integer g<a>(foo<a> x, foo<a> y); */

/*@ logic foo<a> h<a>(foo<a> x); */

/* definition of i should be rejected (free variable in return type) */
/*@ logic foo<a> i<a,b>(foo<b> x); */

//@ predicate bla(foo<int> x) = f(x) == 0 ;

//@ predicate bli(foo<real> x) = g(x,x) == 42 ;

/* blu should be rejected (force unification between two user-introduced
                           type variables)
*/
//@ predicate blu<a,b>(foo<a> x, foo<b> y) = g(x,y) == 36 ;

//@ predicate blu2<a,b>(foo<a> x, foo<b> y) = g(x,x) == 36 && g(y,y) == 72 ;

//@ predicate bar(foo<real> x) = bli(x) && blu2(x,x) ;

// should be rejected
//@ predicate unif1<a,b>(foo<a> x,foo<b>y) = h(x) == h(y);

// should be rejected
//@ logic foo<a> unif2<a,b>(foo<a> x, foo<b>y) = h(y);
