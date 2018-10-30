/* run.config_qualif                                                                  
   DONTRUN: test under construction                                                   
*/

/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-prop="-qed_ko"
   OPT: -wp -wp-par 1 -wp-prop qed_ko -wp-steps 50
*/

/* run.config 
   DONTRUN: test under construction
*/

/*@ ensures qed_ok: \result==((x>y)?x:y);
 */
int max(int x, int y) {
  return (x > y) ? x : y;
}

/*@ ensures qed_ok: \result==((x>0)?x:-x); */
int absolue(int x) {
  return (x >= 0) ? x : -x;
}

/*@ ensures qed_ok: *x == \old(*y) && *y == \old(*x); */
void echange_valeur(int * x, int * y) {
  int a = *x;
  *x = *y;
  *y = a;
}

/*@ ensures qed_ko: x == \old(y) && y == \old(x);
  @*/
void echange_valeur_false(int x, int y) {
  int a = x;
  x = y;
  y = a;
}

/*@ requires a>=0 ;
  ensures \result == a*b; */
int produit(int a, int b) {
  int x = a, y = 0;
  /*@ loop invariant x*b+y==a*b && x>=0;*/
  while (x > 0) {
    y += b;
    --x;
  }
  return (y);
}

/*@ requires n>=0;
  @ ensures (\result*\result) <= n < (\result+1)*(\result+1);
  @*/
int racine(int n) {
  int r = 0;
  /*@ loop invariant r*r <= n;
    @*/
  while (n >= (r+1)*(r+1))
    r = r + 1;
  return(r);
}

/*@ requires n>=0;
  @ ensures (\result*\result) <= n < (\result+1)*(\result+1);
  @*/
int racine_defaut(int n) {
  int r = (n/2) + 1;
  /*@ loop invariant ONLY_PROVED_BY_CVC3: n<(r+1)*(r+1) ;*/
  while (r*r > n)
    --r;
  return(r);
}



/*@ inductive is_power(integer x, integer y, integer p) {
  @ case zero:
  @ \forall integer x; is_power(x, 0, 1);
  @ case non_zero:
  @ \forall integer x,y,p; is_power(x,y-1,p) ==> is_power(x,y,p*x);
  @ }
  @*/

/*@ requires b>=0 ;
  ensures UNSUPPORTED_INDUCTIVE_PREDICATE: is_power(a,b,\result);
 */
int puissance(int a, int b) {
  int p = 1, i = 1;
  /*@ loop invariant is_power(a,i-1,p) && i<=b+1; */
  while (i <= b) {
    p *= a; ++i;
  }
  return (p);
}

/*@ inductive is_fibo(integer n,integer f) {
  case zero: is_fibo(0,0);
  case one: is_fibo(1,1);
  case other:
  \forall integer n,f1,f2; n>1 ==> is_fibo(n-1,f1) ==> is_fibo(n-2,f2) ==>
  is_fibo(n,f1+f2);
}
*/
/*@ requires n >=0 ;
  ensures is_fibo(n,\result) ;*/
int fibonacci(int n) {
  int x = 0;
  int y, k, t;
  if (n != 0) {
    y = x; x = 1; k = 1;
    /*@ loop invariant is_fibo(k,x) && is_fibo(k-1,y) && k<=n; */
    while(k<n) {
      t = y; y = x; x += t; ++k;
    }
  }
  return x;
}


int fact(int n) {
  int i = 1, f = 1;
  while (i <= n) {
    f *= i; ++i;
  }
  return (f);
}

/*@ requires \valid(a+(0..n-1)) && \valid(b+(0..n-1));
    requires n>0;
  behavior same:
   assumes \forall integer i; 0<=i<n ==> a[i] == b[i] ;
   ensures \result == 1;
  behavior different:
     assumes \exists integer i; 0<=i<n && a[i] != b[i] ;
     ensures \result == 0;
  disjoint behaviors; // PROVABLE WITHOUT ARRAYS
  complete behaviors; // PROVABLE WITHOUT ARRAYS
 */
int comparer(int * a, int * b, int n) {
  int i = 0;
  /*@ loop invariant qed_ok: 0<=i<=n && \forall integer k; 0<=k<i ==> a[k]==b[k] ;*/
  while (i < n) {
    if (a[i] != b[i])
      return 0;
    ++i;
  }
  return 1;
}


/*@ requires \valid(a+(0..n-1));
  ensures \forall integer i; 0<=i<n ==> a[\result]<=a[i];

 */
int min_element(int * a, int n) {
  int i = 0, imin = i;
  /*@ loop invariant qed_ok: \forall integer k; 0<=k<i ==> a[imin]<=a[k];
   */
  while (i < n) {
    imin = (a[i] < a[imin] ? i : imin);
    ++i;
  }
  return imin;
}

/*@ predicate is_divisible(integer p,integer q) =
  \exists integer k; p==k*q;
 predicate pgcd(integer a,integer b,integer q) =
  is_divisible(a,q) &&
  is_divisible(b,q) &&
  \forall integer k;
   is_divisible(a,k) && is_divisible(b,k) ==> is_divisible(q,k);

 lemma pgcd_refl: \forall integer x; pgcd(x,x,x); // HOW TO PROVE IT????
 lemma pgcd_minus:\forall integer x,y,k; pgcd(x,y,k)<==>pgcd(x,y-x,k);// HOW TO PROVE IT????
 lemma pgcd_minus_2:\forall integer x,y,k; pgcd(x,y,k)<==>pgcd(x-y,y,k);// HOW TO PROVE IT????
*/
/*@
  requires a>0 && b>0;
  ensures pgcd(a,b,\result); // INVALID GOAL GENERATED IN THE GUI IF THE LOOP INVARIANT IS PROVED FIRST
*/
int pgcd(int a,int b) {
  int x = a, y = b;
  /*@ loop invariant \forall integer k; pgcd(x,y,k) ==> pgcd(a,b,k);
    loop variant (x-y>=0)?(x-y):(y-x);
*/
  while (x != y) {
    if (x > y)
      x -= y;
    else
      y -= x;
  }
  return (y);
}


/*@ predicate is_prime(integer n) =
   n!=1 &&
   \forall integer k; k>=0 ==> is_divisible(n,k) ==> k==1 || k==n;

 lemma div_mod:
   \forall integer a,b; is_divisible(a,b) <==> a%b==0; */

/*@ requires n>=0;
  behavior is_prime:
   assumes is_prime(n);
   ensures \result==1;
 behavior not_prime:
  assumes !is_prime(n);
  ensures \result==0;
*/
int premier (int n) {
  if (n<2)
    return 0;
  if (n==2)
    return 1;
  int i = 2;
  /*@ loop invariant \forall integer k; 2<=k<i ==> !is_divisible(n,k);
    loop invariant 2<=i<n;
    loop variant n-i;*/
  while((i<=racine(n)) && (n%i != 0))
    ++i;
  return (n%i!=0);
}
