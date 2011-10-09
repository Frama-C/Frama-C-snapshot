typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef long long int64_t;

typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;

struct S0 {
   int8_t f0;
   int16_t f1;
   int64_t f2;
   uint16_t f3;
   int8_t f4;
   int32_t f5;
   int16_t f6;
   int32_t f7;
   int16_t f8;
};

struct S2 {
   int8_t f0;
    const int16_t f1;
   int16_t f2;
   int32_t f3;
   uint8_t f4;
   struct S0 f5;
   int64_t f6;
   int8_t f7;
   int16_t f8;
};

struct S1 {
   int32_t f0;
   uint8_t f1;
};

struct S3 {
   struct S2 f0;
    const uint32_t f1;
    const uint32_t f2;
   int64_t f3;
   struct S0 f4;
    const struct S1 f5;
   int8_t f6;
    const int8_t f7;
};


struct S0 g_3 = {-1L,0x4B54L,6L,7L,0xFFL,1L,-10L,0x67457993L,0x3C7DL};
struct S3 g_8 = {{0xD5L,-10L,0L,0x900B0881L,0xDAL,{0xDBL,0x846BL,1L,-7L,0xF3L,0xFC0336AEL,6L,0x52E4A6B2L,0x4EB0L},0x117216709E149CFFLL,0x9CL,-1L},0x1636717BL,-4L,4L,{0xE3L,0xECDCL,0xF1FA6F63EEDA781BLL,0xF7A0L,0x7CL,0L,0xA77DL,0x7FC7DF39L,0x3C5AL},{0xA104ACD6L,0xA8L},0xADL,8L};

main(){
  Frama_C_dump_each();
  return 0;
}
