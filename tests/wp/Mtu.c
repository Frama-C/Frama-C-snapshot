/* run.config 
   DONTRUN: test under construction
*/

/*
note: property named "c" is also decomposed into 5 properties 
      named "case1"..."case5"
*/
typedef struct {int a,b,c ; } TMtu;
/*@ 
  assigns pt->c ;

behavior oracle_ok:
  ensures result: oracle_ok:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    ((x==0)?\result==0
           :((x==p)?\result==e
                   :\result!=0)) ;

  ensures c: oracle_ok:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    ((x==0)?((e==0)?pt->c==p
                   :pt->c==0)
           :((x==p)?((e!=0)?pt->c==p-1
                           :pt->c==p)
                   :pt->c==x-1)) ;
  ensures case1: oracle_ok: 
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x==0 && e==0 ==> pt->c==p ;
  ensures case2: oracle_ok:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x==0 && e!=0 ==> pt->c==0 ;
  ensures case3: oracle_ok:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x!=0 && x==p && e!=0 ==> pt->c==p-1 ;
  ensures case4: oracle_ok:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x!=0 && x==p && e==0 ==> pt->c==p ;
  ensures case5: oracle_ok:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x!=0 && x!=p ==> pt->c==x-1 ;

behavior oracle_ko:
  ensures result: oracle_ko:
    \let x = ((c==0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    ((x==0)?\result==0
           :((x==p)?\result==e
                   :\result!=0)) ;

  ensures c: oracle_ko:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    ((x!=0)?((e==0)?pt->c==p
                   :pt->c==0)
           :((x==p)?((e!=0)?pt->c==p-1
                           :pt->c==p)
                   :pt->c==x-1)) ;
  ensures case1: oracle_ko:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x==0 && e==0 ==> pt->c==\old(pt->c) ;
  ensures case2: oracle_ko:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x==0 && e!=0 ==> pt->c!=0 ;
  ensures case3: oracle_ko:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x!=0 && x==p && e!=0 ==> pt->c==p ;
  ensures case4: oracle_ko:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x!=0 && x==p && e==0 ==> pt->c==p-1 ;
  ensures case5: oracle_ko:
    \let x = ((c!=0)?((e!=0)?(p-m)
                            :0)
                    :\old(pt->c)) ;
    x!=0 && x!=p ==> pt->c==x ;
*/
int Mtu( TMtu *pt, int e, int c, int p, int m ) {
   int r;

   if (c)
      if (e)
         pt->c = p - m;
      else
         pt->c = 0;

   if (!pt->c) {
      r = 0;
      if ( !e )
         pt->c = p;
      }
   else if (pt->c == p) {
      r = e;
      if (e)
         pt->c = pt->c - 1;
      }
   else {
      r = 1;
      pt->c = pt->c - 1;
   }
   return r;
}
