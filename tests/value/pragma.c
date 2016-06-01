#pragma section DATA ".CRCPPRAM_TIME_I" ".CRCPPRAM_TIME"  far-absolute
#pragma section DATA ".IRAM_BIT_I" ".IRAM_BIT"   near-data
#pragma section CONST  ".CA_ROM" ".CA_ROM" far-absolute
#pragma section SCONST ".CA_ROM" ".CA_ROM" far-absolute
#pragma option -Xpragma-section-last
#pragma option -Xsmall-const=0
#pragma section CODE ".illegal_code" standard RX
#pragma section CONST ".illegal_const" ".illegal_const" far-absolute R
#pragma section SCONST ".illegal_sconst" ".illegal_sconst" near-code R
#pragma section STRING ".illegal_string" far-absolute R
#pragma section DATA ".illegal_data" ".illegal_bss" near-data RW
#pragma section SDATA ".illegal_sdata" ".illegal_sbss" near-data RW
#pragma section CODE ".illegal_code" standard RX


void main () {
  return;
}
