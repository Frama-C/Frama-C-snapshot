/* run.config
   OPT: -load-script tests/misc/bts0489.ml
*/

typedef unsigned int uint8_t;
typedef int int8_t;
typedef unsigned int uint16_t;
typedef int int16_t;
typedef int int32_t;
typedef unsigned int uint32_t;

void foo1(uint8_t x) {};

int16_t t1(void)
{
  uint8_t u8a, u8b, u8c;
  int8_t s8a, s8b;
  uint16_t u16a;
  int16_t s16a;
  int32_t s32a;
  float f32a;
  double f64a;
   foo1(u8a);            /* compliant     */
   foo1(u8a + u8b);      /* compliant     */
   foo1(s8a);            /* not compliant */
   foo1(u16a);           /* not compliant */
   foo1(2);              /* not compliant */
   foo1(2U);             /* compliant     */
   foo1((uint8_t)2);     /* compliant     */
   /*... s8a + u8a         /* not compliant */
   /*... s8a + (int8_t)u8a /* compliant     */
   s8b = u8a;            /* not compliant */
   /*... u8a + 5           /* not compliant */
   /*... u8a + 5U          /* compliant     */
   /*... u8a + (uint8_t)5  /* compliant     */
   u8a = u16a;           /* not compliant */
   u8a = (uint8_t)u16a;  /* compliant     */
   u8a = 5UL;            /* not compliant */
   /*... u8a + 10UL        /* compliant     */
   u8a = 5U;             /* compliant     */
   /*... u8a + 3           /* not compliant */
   /*... u8a >> 3          /* compliant     */
   /*... u8a >> 3U         /* compliant     */

   /*... s32a + 80000      /* compliant     */
   /*... s32a + 80000L     /* compliant     */
   f32a = f64a;          /* not compliant */
   f32a = 2.5;           /* not compliant -
                            unsuffixed floating
                            constants are of type
                            double        */
   u8a  = u8b +  u8c;    /* compliant     */
   s16a = u8b +  u8b;    /* not compliant */
   s32a = u8b +  u8c;    /* not compliant */
   f32a = 2.5F;          /* compliant     */
   u8a  = f32a;          /* not compliant */
   s32a = 1.0;           /* not compliant */
   s32a = u8b +  u8c;    /* not compliant */
   f32a = 2.5F;          /* compliant     */
   u8a  = f32a;          /* not compliant */
   s32a = 1.0;           /* not compliant */
   f32a = 1;             /* not compliant */
   f32a = s16a;          /* not compliant */
   /*... f32a + 1                  /* not compliant */
   /*... f64a * s32a               /* not compliant */
   /*...*/
   return (s32a);                /* not compliant */
   /*...*/
   return (s16a);                /* compliant     */
   /*...*/
   return (20000);               /* compliant     */
   /*...*/
   return (20000L);              /* not compliant */
   /*...*/
   return (s8a);                 /* not compliant */
   /*...*/
   return (u16a);                /* not compliant */
};

int16_t foo2(void)
{
  uint8_t u8a, u8b;
  int8_t s8a;
  uint16_t u16a,u16b;
  int16_t s16a,s16b;
  int32_t s32a,s32b;
  uint32_t u32a;
  float f32a,f32b;
  double f64a,f64b ;

   /*... (u16a + u16b) + u32a      /* not compliant */
   /*... s32a + s8a + s8b          /* compliant     */
   /*... s8a + s8b + s32a          /* not compliant */
   f64a = f32a + f32b;           /* not compliant */
   f64a = f64b + f32a;           /* compliant     */
   f64a = s32a / s32b;           /* not compliant */
   u32a = u16a + u16a;           /* not compliant */
   s16a = s8a;                   /* compliant     */
   s16a = s16b + 20000;          /* compliant     */
   s32a = s16a + 20000;          /* not compliant */
   s32a = s16a + (int32_t)20000; /* compliant     */
   u16a = u16b + u8a;            /* compliant     */
   foo1(u16a);                   /* not compliant */
   foo1(u8a + u8b);              /* compliant     */
   /*...*/
   return s16a;                  /* compliant     */
   /*...*/
   return s8a;                   /* not compliant */
}
