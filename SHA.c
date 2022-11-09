#include <stdio.h>
#include <stdint.h>
#include <string.h>


#define MESSAGE     "abcdefg"

#define Print printf

typedef enum {
  SUCCESS
  
} STATUS;

uint32_t K[] = {
     0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
     0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
     0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
     0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
     0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
     0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
     0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
     0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 };


uint32_t H[] = {
     0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 };


STATUS
MessagePreprocess (
  char      *String,
  uint32_t  **Data
  )
{
  uint64_t StringLen;
  uint64_t NewStringLen;
  char *NewString;
  int NumberOfZeroByte;
  int DataLen;
  int DataUnit;
  int i, j;

  DataUnit = sizeof(uint32_t);
  StringLen = (uint64_t) strlen(String);

  NewString = malloc ((StringLen + 1) * sizeof(char));
  NewStringLen = StringLen + 1;
  strcpy(NewString, String);
  NewString[StringLen] = 0x80;
  NewString[StringLen+1] = '\0';
  Print ("NewString: %s\n", NewString);

  NumberOfZeroByte = 64 - (StringLen + 1 + 8) % 64; 

  Print ("String Length: %lld\n", StringLen);
  Print ("Number Of Zero Bit: %d\n", NumberOfZeroByte);
  DataLen = (StringLen + 1 + NumberOfZeroByte + 8) / DataUnit;
  Print ("Data Length: %d\n", DataLen);


  *Data = (uint32_t*) malloc (DataLen * DataUnit);
  for (i = 0 ; i < (DataLen - 2) ; i++) {
    (*Data)[i] = 0;
  }
  for (i = 0 ; i < (NewStringLen / DataUnit) ; i++) {
    (*Data)[i] = NewString[DataUnit * i] << 24 |
                 NewString[DataUnit * i + 1] << 16 |
                 NewString[DataUnit * i + 2] << 8 |
                 NewString[DataUnit * i + 3] & 0xff;
  }

  if (NewStringLen % DataUnit != 0) {
    (*Data)[i] = 0;
    for (j = 0 ; j < NewStringLen % DataUnit ; j++) {
      (*Data)[i] |= (NewString[DataUnit * i + j] & 0xFF) << (24 - j * 8);
    }
  }
  free (NewString);

  /*
  for (i = DataLen - 8 ; i < DataLen ; i++) {
    Print ("StringLen 0x%llx\n", StringLen);
    (*Data)[i] = (StringLen & 0xFF00000000000000) >> 56;
    StringLen <<= 8;
  }
  */
  (*Data)[DataLen - 2] = (uint32_t) (StringLen * 8) >> 16;
  (*Data)[DataLen - 1] = (uint32_t) (StringLen * 8) & 0xFFFF;

  for (i = 0 ; i < DataLen ; i++) {
    if (i % 16 == 0) {
      Print ("\n");
    }
    Print (" 0x%08x", (*Data)[i]);
  }
/*
  for (i = 0 ; i < DataLen - 8 ; i++) {
    (*Data)[i] = 0;
  }
  (*Data)[0] = 0x80;
  (*Data)[3] = 'a';
  (*Data)[2] = 'b';
  (*Data)[1] = 'c';

  for (i = 0 ; i < DataLen ; i++) {
    if (i % 16 == 0) {
      Print ("\n");
    }
    Print (" 0x%02x", (*Data)[i]);
  }
*/

  return SUCCESS;
}

uint32_t RightRotate(uint32_t n, uint32_t d)
{
   /* In n>>d, first d bits are 0. To put last 3 bits of at
     first, do bitwise or of n>>d with n <<(INT_BITS - d) */
   return (n >> d)|(n << (32 - d));
}


STATUS
HashComputation (
  uint32_t  *Data
  )
{
  int i;
  uint32_t w[16];
  uint32_t t0;
  uint32_t t1;
  uint32_t s0;
  uint32_t s1;
  uint32_t temp1,temp2;
  uint32_t maj,ch;

  
  for (i = 0 ; i < 16 ; i++) {
    w[i] = Data[i];
//    Print("%x\n",w[i]);
  }
  for (i = 16 ; i < 64 ; i++) {
    s0 = RightRotate(w[i-15], 7) ^ RightRotate(w[i-15], 18) ^ (w[i-15] >> 3);
    s1 = RightRotate(w[i-2], 17) ^ RightRotate(w[i-2], 19) ^ (w[i-2] >> 10);
    w[i] = w[i-16] + s0 + w[i-7] + s1;
//    Print("%x\n",w[i]);
  }

  uint32_t a = H[0];
  uint32_t b = H[1];
  uint32_t c = H[2];
  uint32_t d = H[3];
  uint32_t e = H[4];
  uint32_t f = H[5];
  uint32_t g = H[6];
  uint32_t h = H[7];


  
  for (i = 0 ; i < 64 ; i++) {
    t1 = RightRotate(e, 6) ^ RightRotate(e, 11) ^ RightRotate(e, 25);
    ch = (e & f) ^ ((~e) & g);
    temp1 = h + t1 + ch + K[i] + w[i];
    t0 = RightRotate(a, 2) ^ RightRotate(a, 13) ^ RightRotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    temp2 = t0 + maj;

    h = g;
    g = f;
    f = e;
    e = (d + temp1) >> 0;
    d = c;
    c = b;
    b = a;
    a = (temp1 +temp2) >> 0;
  }

  H[0] = (H[0] + a) >> 0;
  H[1] = (H[1] + b) >> 0;
  H[2] = (H[2] + c) >> 0;
  H[3] = (H[3] + d) >> 0;
  H[4] = (H[4] + e) >> 0;
  H[5] = (H[5] + f) >> 0;
  H[6] = (H[6] + g) >> 0;
  H[7] = (H[7] + h) >> 0;
  
  Print("\n");
  for(i = 0 ; i < 8 ; i++) {
    Print("%x",H[i]);
  }
  Print("\n");

  return SUCCESS;
}

void
PrintHashNumber()
{
  int i;
  for(i = 0 ; i < 8 ; i++) {
    Print("%x",H[i]);
  }
  Print("\n");
}

int main(void)
{
  uint8_t *Data;
  int i;
  
//  PrintHashNumber();

  MessagePreprocess (MESSAGE, &Data);

  HashComputation((uint32_t*)Data);
  free (Data);

  PrintHashNumber();
/*
  Print("QQQ\n");
  for(i = 0 ; i < 8 ; i++) {
    Print("%x",H[i]);
  }
  Print("\n");
*/
}

