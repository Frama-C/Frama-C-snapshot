void stop(void) __attribute__ ((noreturn)) ;

int haltme(void) __attribute__ ((noreturn)) ;


void never_ends(void)  __attribute__ ((noreturn)) 
{  while(1) ;
 return;
}; 

void should_never_end(int c)  __attribute__ ((noreturn)) 
{ 
  if (c) while(1) ;} ;

void warn_never_ends(void)
{ while(1) ;} ;

void warn_may_never_end(int c)
{ 
  if (c) while(1) ;} ;

static volatile int v=55,w=66;
int main(int c) {
  int x=0;

  if (v) warn_may_never_end (v);
  if (v) warn_may_never_end (1);
  if (v) warn_never_ends ();
  if (v) stop();
  if (v) x = haltme ();
  if (v) never_ends ();
  if (v) should_never_end (v);
  if (v) should_never_end (1);

  return x;
}

