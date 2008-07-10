
typedef struct rc4_key_st
{
  unsigned char x,y;
  unsigned char data[256];
} RC4_KEY;

   
/*@ axiom valid_range_ax1a:
  @	\forall unsigned long k; k>=0 => 0<=(k>>3L)<=k
  @*/

/*@ axiom valid_range_ax1b:
  @	\forall unsigned long k; k>=0 => 0<=((k>>3L)*8)<=k
  @*/

/*@ axiom valid_range_ax1c:
  @	\forall unsigned long k; k>=0 => 0<= (k&0x07) <= 7
  @*/

/*@ axiom valid_range_ax1d:
  @	\forall unsigned long k; k>=0 => ((k>>3L)*8 + (k&0x07) == k)
  @*/
   
/*@ axiom valid_range_ax2:
  @	\forall unsigned char k; 0 <= (k&0xff) < 256
  @*/	
   
/*@ axiom valid_range_ax4:
  @	\forall unsigned char inp; \forall unsigned char k; 
  @	\forall unsigned char out; out == (k ^ inp) => 0<=out<256
  @*/
     
  
/*@ requires len >= 0 && 
  @ 	\valid(key) && \valid_range(d,0,255) &&
  @		\valid_range(indata,0,len-1) &&  \valid_range(outdata,0,len-1)	
  @*/
void RC4(RC4_KEY *key,register unsigned char *d, unsigned long len, const unsigned char *indata,
	 unsigned char *outdata)
{
  //register unsigned char *d;
  register unsigned char x,y,tx,ty;
  int i;

  x=key->x;
  y=key->y;
  //d=key->data;
         
#define LOOP(in,out)				\
  x=((x+1)&0xff);				\
  tx=d[x];					\
  y=(tx+y)&0xff;				\
  d[x]=ty=d[y];					\
  d[y]=tx;					\
  (out) = d[(tx+ty)&0xff]^ (in);

#define RC4_LOOP(a,b,i)	LOOP(a[i],b[i])

  i=(int)(len>>3L);
  if (i)
    {
      /*@ invariant i>0 && i<=len && 
	@   \valid_range(indata,0,7) && \valid_range(outdata,0,7) &&
	@   indata == \old(indata)+((len>>3L)-i)*8 && 
	@   outdata == \old(outdata)+((len>>3L)-i)*8 
	@ variant i
	@*/	
      while(1) //for (;;)
	{
	  RC4_LOOP(indata,outdata,0);  
	  RC4_LOOP(indata,outdata,1); 
	  RC4_LOOP(indata,outdata,2);
	  RC4_LOOP(indata,outdata,3);
	  RC4_LOOP(indata,outdata,4);
	  RC4_LOOP(indata,outdata,5);
	  RC4_LOOP(indata,outdata,6);
	  RC4_LOOP(indata,outdata,7); 
	  indata+=8;
	  outdata+=8;
	  if (--i == 0) break;
	}
    }
  /*@ assert i==0 &&
    @   indata == \old(indata)+((len>>3L)*8) && 
    @   outdata == \old(outdata)+((len>>3L)*8)
    @*/
		
  i=(int)len&0x07;
  /*@ assert i== (len&0x07) && \valid_range(indata,0,i-1) &&
    @				 \valid_range(outdata,0,i-1)
    @*/
		
  if(i>0){ RC4_LOOP(indata,outdata,0);
    if (--i > 0){ RC4_LOOP(indata,outdata,1);
      if (--i > 0) { RC4_LOOP(indata,outdata,2); 
	if (--i > 0) { RC4_LOOP(indata,outdata,3); 
	  if (--i > 0) { RC4_LOOP(indata,outdata,4); 
	    if (--i > 0) { RC4_LOOP(indata,outdata,5); 
	      if (--i > 0) { RC4_LOOP(indata,outdata,6);}}}}}}
  }	
		
  key->x=x;
  key->y=y;
}


    
/*
  Local Variables: 
  compile-command: "make rc4.gui"
  End: 
*/
