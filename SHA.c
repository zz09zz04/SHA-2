#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>


#define MESSAGE     ""

#define Print printf

typedef enum {
  SUCCESS
  
} STATUS;

uint32_t K_32[] = {
     0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
     0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
     0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
     0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
     0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
     0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
     0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
     0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 };


uint64_t K_64[] = {
            0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc, 0x3956c25bf348b538, 
            0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118, 0xd807aa98a3030242, 0x12835b0145706fbe, 
            0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2, 0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 
            0xc19bf174cf692694, 0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65, 
            0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5, 0x983e5152ee66dfab, 
            0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4, 0xc6e00bf33da88fc2, 0xd5a79147930aa725, 
            0x06ca6351e003826f, 0x142929670a0e6e70, 0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 
            0x53380d139d95b3df, 0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b, 
            0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30, 0xd192e819d6ef5218, 
            0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8, 0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 
            0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8, 0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 
            0x682e6ff3d6b2b8a3, 0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec, 
            0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b, 0xca273eceea26619c, 
            0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178, 0x06f067aa72176fba, 0x0a637dc5a2c898a6, 
            0x113f9804bef90dae, 0x1b710b35131c471b, 0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 
            0x431d67c49c100d4c, 0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817 };


uint32_t H_32[8];

uint32_t H_256[] = {
     0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 };


uint32_t H_224[] = {
     0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939, 0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4 };


uint64_t H_64[8];

uint64_t H_512[] = {
           0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1, 
           0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179 };

uint64_t H_384[] = {
           0xcbbb9d5dc1059ed8, 0x629a292a367cd507, 0x9159015a3070dd17, 0x152fecd8f70e5939, 
           0x67332667ffc00b31, 0x8eb44a8768581511, 0xdb0c2e0d64f98fa7, 0x47b5481dbefa4fa4 };

void
InitializeConstant (
  void
  )
{
  int i; 
  for (i = 0 ; i < 8 ; i++) {
    H_32[i] = H_256[i];
    H_64[i] = H_512[i];
  }
}

STATUS
MessagePreprocess32Bit (
  char      *String,
  uint32_t  **Data,
  uint64_t  *DataLen
  )
{
  uint64_t StringLen;
  uint64_t NewStringLen;
  char *NewString;
  int NumberOfZeroByte;
//  int DataLen;
  int DataUnit;
  int i, j;

  DataUnit = sizeof(uint32_t);
  StringLen = (uint64_t) strlen(String);

  NewString = malloc ((StringLen + 2) * sizeof(char));
  NewStringLen = StringLen + 1;
  strcpy(NewString, String);
  NewString[StringLen] = 0x80;
  NewString[StringLen+1] = '\0';
  Print ("NewString: %s\n", NewString);

  NumberOfZeroByte = 64 - (StringLen + 1 + 8) % 64; 

  Print ("String Length: %lld\n", StringLen);
  Print ("Number Of Zero Bit: %d\n", NumberOfZeroByte);
  *DataLen = (uint64_t)(StringLen + 1 + NumberOfZeroByte + 8) / DataUnit;
  Print ("Data Length: %lld\n", *DataLen);


  *Data = (uint32_t*) malloc (*DataLen * DataUnit);
  for (i = 0 ; i < (*DataLen - 2) ; i++) {
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
  (*Data)[*DataLen - 2] = (uint32_t) (StringLen * 8) >> 16;
  (*Data)[*DataLen - 1] = (uint32_t) (StringLen * 8) & 0xFFFF;

  for (i = 0 ; i < *DataLen ; i++) {
    if (i % 4 == 0) {
      Print ("\n");
    }
    Print (" 0x%08x", (*Data)[i]);
  }
  Print("\n");
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


STATUS
MessagePreprocess64Bit (
  char      *String,
  uint64_t  **Data,
  uint64_t  *DataLen
  )
{
  uint64_t StringLen;
  uint64_t NewStringLen;
  char *NewString;
  int NumberOfZeroByte;
//  int DataLen;
  int DataUnit;
  int i, j;

  DataUnit = sizeof(uint64_t);
  StringLen = (uint64_t) strlen(String);

  NewString = malloc ((StringLen + 2) * sizeof(char));
  NewStringLen = StringLen + 1;
  strcpy(NewString, String);
  Print ("DataUnit: %lld\n", DataUnit);
  NewString[StringLen] = 0x80;
  NewString[StringLen+1] = '\0';
  Print ("NewString: %s\n", NewString);

  NumberOfZeroByte = 128 - (StringLen + 1 + 8) % 128; 

  Print ("String Length: %lld\n", StringLen);
  Print ("Number Of Zero Bit: %d\n", NumberOfZeroByte);
  *DataLen = (uint64_t)(StringLen + 1 + NumberOfZeroByte + 8) / DataUnit;
  Print ("Data Length: %lld\n", *DataLen);


  *Data = (uint64_t*) malloc (*DataLen * DataUnit);
  for (i = 0 ; i < (*DataLen - 1) ; i++) {
    (*Data)[i] = 0;
  }
  for (i = 0 ; i < (NewStringLen / DataUnit) ; i++) {
    (*Data)[i] = (uint64_t)NewString[DataUnit * i] << 56 |
                 (uint64_t)NewString[DataUnit * i + 1] << 48 |
                 (uint64_t)NewString[DataUnit * i + 2] << 40 |
                 (uint64_t)NewString[DataUnit * i + 3] << 32 |
                 NewString[DataUnit * i + 4] << 24 |
                 NewString[DataUnit * i + 5] << 16 |
                 NewString[DataUnit * i + 6] << 8 |
                 NewString[DataUnit * i + 7] & 0xff;
  }

  if (NewStringLen % DataUnit != 0) {
    (*Data)[i] = 0;
    for (j = 0 ; j < NewStringLen % DataUnit ; j++) {
      (*Data)[i] |= (uint64_t)(NewString[DataUnit * i + j] & 0xFF) << (56 - j * 8);
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
//  (*Data)[*DataLen - 2] = (uint32_t) (StringLen * 8) >> 16;
//  (*Data)[*DataLen - 1] = (uint32_t) (StringLen * 8) & 0xFFFF;
  (*Data)[*DataLen - 1] = (uint64_t) (StringLen * 8) & 0xFFFFFFFF;

  for (i = 0 ; i < *DataLen ; i++) {
    if (i % 4 == 0) {
      Print ("\n");
    }
    Print (" 0x%016llx", (*Data)[i]);
  }
  Print("\n");
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

uint32_t RightRotate32Bit(uint32_t n, uint32_t d)
{
   /* In n>>d, first d bits are 0. To put last 3 bits of at
     first, do bitwise or of n>>d with n <<(INT_BITS - d) */
   return (n >> d)|(n << (32 - d));
}


uint64_t RightRotate64Bit(uint64_t n, uint64_t d)
{
   /* In n>>d, first d bits are 0. To put last 3 bits of at
     first, do bitwise or of n>>d with n <<(INT_BITS - d) */
   return (n >> d)|(n << (64 - d));
}


STATUS
HashComputation32Bit (
  uint32_t  *MegData,
  uint64_t  DataLen
  )
{
  int i;
  uint32_t w[64];
  uint32_t t0;
  uint32_t t1;
  uint32_t s0;
  uint32_t s1;
  uint32_t temp1,temp2;
  uint32_t maj,ch;
  uint64_t n;
  uint32_t *Data;
  Data = MegData;
  
  for (n = 0 ; n < DataLen / 16 ; n++) {
    Data += (n*16);
    for (i = 0 ; i < 16 ; i++) {
      w[i] = Data[i];
    }
    for (i = 16 ; i < 64 ; i++) {
      s0 = RightRotate32Bit(w[i-15], 7) ^ RightRotate32Bit(w[i-15], 18) ^ (w[i-15] >> 3);
      s1 = RightRotate32Bit(w[i-2], 17) ^ RightRotate32Bit(w[i-2], 19) ^ (w[i-2] >> 10);
      w[i] = w[i-16] + s0 + w[i-7] + s1;
    }

    uint32_t a = H_32[0];
    uint32_t b = H_32[1];
    uint32_t c = H_32[2];
    uint32_t d = H_32[3];
    uint32_t e = H_32[4];
    uint32_t f = H_32[5];
    uint32_t g = H_32[6];
    uint32_t h = H_32[7];

    for (i = 0 ; i < 64 ; i++) {
      t1 = RightRotate32Bit(e, 6) ^ RightRotate32Bit(e, 11) ^ RightRotate32Bit(e, 25);
      ch = (e & f) ^ ((~e) & g);
      temp1 = h + t1 + ch + K_32[i] + w[i];
      t0 = RightRotate32Bit(a, 2) ^ RightRotate32Bit(a, 13) ^ RightRotate32Bit(a, 22);
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

    H_32[0] = (H_32[0] + a) >> 0;
    H_32[1] = (H_32[1] + b) >> 0;
    H_32[2] = (H_32[2] + c) >> 0;
    H_32[3] = (H_32[3] + d) >> 0;
    H_32[4] = (H_32[4] + e) >> 0;
    H_32[5] = (H_32[5] + f) >> 0;
    H_32[6] = (H_32[6] + g) >> 0;
    H_32[7] = (H_32[7] + h) >> 0;
  }
/*  
  Print("\n");
  for(i = 0 ; i < 8 ; i++) {
    Print("%x",H[i]);
  }
  Print("\n");
*/
  return SUCCESS;
}


STATUS
HashComputation64Bit (
  uint64_t  *MegData,
  uint64_t  DataLen
  )
{
  int i;
  uint64_t w[80];
  uint64_t t0;
  uint64_t t1;
  uint64_t s0;
  uint64_t s1;
  uint64_t temp1,temp2;
  uint64_t maj,ch;
  uint64_t n;
  uint64_t *Data;
  Data = MegData;
  
  for (n = 0 ; n < DataLen / 16 ; n++) {
    Data += (n*16);
    for (i = 0 ; i < 16 ; i++) {
      w[i] = Data[i];
    }
    for (i = 16 ; i < 80 ; i++) {
      s0 = RightRotate64Bit(w[i-15], 1) ^ RightRotate64Bit(w[i-15], 8) ^ (w[i-15] >> 7);
      s1 = RightRotate64Bit(w[i-2], 19) ^ RightRotate64Bit(w[i-2], 61) ^ (w[i-2] >> 6);
      w[i] = w[i-16] + s0 + w[i-7] + s1;
    }
  Print ("debug\n");

    uint64_t a = H_64[0];
    uint64_t b = H_64[1];
    uint64_t c = H_64[2];
    uint64_t d = H_64[3];
    uint64_t e = H_64[4];
    uint64_t f = H_64[5];
    uint64_t g = H_64[6];
    uint64_t h = H_64[7];

    for (i = 0 ; i < 80 ; i++) {
      t1 = RightRotate64Bit(e, 14) ^ RightRotate64Bit(e, 18) ^ RightRotate64Bit(e, 41);
      ch = (e & f) ^ ((~e) & g);
      temp1 = h + t1 + ch + K_64[i] + w[i];
      t0 = RightRotate64Bit(a, 28) ^ RightRotate64Bit(a, 34) ^ RightRotate64Bit(a, 39);
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

    H_64[0] = (H_64[0] + a) >> 0;
    H_64[1] = (H_64[1] + b) >> 0;
    H_64[2] = (H_64[2] + c) >> 0;
    H_64[3] = (H_64[3] + d) >> 0;
    H_64[4] = (H_64[4] + e) >> 0;
    H_64[5] = (H_64[5] + f) >> 0;
    H_64[6] = (H_64[6] + g) >> 0;
    H_64[7] = (H_64[7] + h) >> 0;
  }
/*  
  Print("\n");
  for(i = 0 ; i < 8 ; i++) {
    Print("%x",H[i]);
  }
  Print("\n");
*/
  return SUCCESS;
}


/*
STATUS
ComputeFullMsgHash (
  uint32_t  *Data,
  uint64_t  DataLen
  )
{
  uint64_t i;
  for (i = 0 ; i < DataLen / 16 ; i++) {
    HashComputation (Data + i*16);
  }
  return SUCCESS;
}
*/
void
PrintHashNumber32Bit()
{
  int i;
  for(i = 0 ; i < 8 ; i++) {
    Print("%08lx",H_32[i]);
  }
  Print("\n");
}

void
PrintHashNumber64Bit()
{
  int i;
  for(i = 0 ; i < 8 ; i++) {
    Print("%016llx",H_64[i]);
  }
  Print("\n");
}


int main(void)
{
  uint32_t *Data;
  uint64_t *Data64;
  uint64_t DataLen;
  int i;

  InitializeConstant ();
  
//  PrintHashNumber();

  MessagePreprocess32Bit (MESSAGE, &Data, &DataLen);

//  HashComputation((uint32_t*)Data);
  HashComputation32Bit (Data, DataLen);
//  ComputeFullMsgHash (Data, DataLen);
  free (Data);

  PrintHashNumber32Bit();

  MessagePreprocess64Bit (MESSAGE, &Data64, &DataLen);
  
  HashComputation64Bit (Data64, DataLen);

  PrintHashNumber64Bit();

  free (Data64);
/*
  Print("QQQ\n");
  for(i = 0 ; i < 8 ; i++) {
    Print("%x",H[i]);
  }
  Print("\n");
*/
}

