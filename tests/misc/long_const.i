void main() {
 unsigned long long i;
 i = 0xFFFF804000000000UL;
 unsigned long j= ((((((256ULL) >> 8) * 0xffff000000000000UL) | (256ULL << 39) )) + (1ULL << 39)/2ULL); 
 CEA_f(sizeof(long),i,j);
}
