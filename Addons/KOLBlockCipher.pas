unit KOLBlockCipher;
{$A1}
interface

uses Windows, Messages, KOL;

type
// PBlockCipher64 = ^TBlockCipher64;
 TBlockCipher64 = object(TObj)
  protected

    IV, CV: array[0..7] of byte;
    procedure IncCounter;

  public
   // code
    procedure InitKey(const Key; Size: longword);virtual;
    
    procedure Reset;
      { Reset any stored chaining information }
    procedure Burn;virtual;
      { Clear all stored key information and chaining information }
    procedure SetIV(const Value);
      { Sets the IV to Value and performs a reset }
    procedure GetIV(var Value);
      { Returns the current chaining information, not the actual IV }
    procedure InitBlockCipher64(const Key; Size: longword; InitVector: pointer);
      { Do key setup based on the data in Key, size is in bits }

    procedure EncryptCBC(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CBC method of encryption }
    procedure DecryptCBC(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CBC method of decryption }
    procedure EncryptCFB8bit(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CFB (8 bit) method of encryption }
    procedure DecryptCFB8bit(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CFB (8 bit) method of decryption }
    procedure EncryptCFBblock(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CFB (block) method of encryption }
    procedure DecryptCFBblock(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CFB (block) method of decryption }
    procedure EncryptOFB(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the OFB method of encryption }
    procedure DecryptOFB(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the OFB method of decryption }
    procedure EncryptCTR(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CTR method of encryption }
    procedure DecryptCTR(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CTR method of decryption }
    procedure EncryptECB(const Indata; var Outdata);virtual;
      { Override it! }
    procedure DecryptECB(const Indata; var Outdata);virtual;
      { Override it! }


  destructor Destroy; virtual;
 end;

type
// PBlockCipher128 = ^TBlockCipher128;
 TBlockCipher128 = object(TObj)
  private
    IV, CV: array[0..15] of byte;

    procedure IncCounter;
  public
    procedure InitKey(const Key; Size: longword);virtual;

    procedure Reset; 
      { Reset any stored chaining information }
    procedure Burn;
      { Clear all stored key information and chaining information }
    procedure SetIV(const Value);
      { Sets the IV to Value and performs a reset }
    procedure GetIV(var Value); 
      { Returns the current chaining information, not the actual IV }
    procedure InitBlockCipher128(const Key; Size: longword; InitVector: pointer);
      { Do key setup based on the data in Key, size is in bits }

    procedure EncryptCBC(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CBC method of encryption }
    procedure DecryptCBC(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CBC method of decryption }
    procedure EncryptCFB8bit(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CFB (8 bit) method of encryption }
    procedure DecryptCFB8bit(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CFB (8 bit) method of decryption }
    procedure EncryptCFBblock(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CFB (block) method of encryption }
    procedure DecryptCFBblock(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CFB (block) method of decryption }
    procedure EncryptOFB(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the OFB method of encryption }
    procedure DecryptOFB(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the OFB method of decryption }
    procedure EncryptCTR(const Indata; var Outdata; Size: longword);
      { Encrypt size bytes of data using the CTR method of encryption }
    procedure DecryptCTR(const Indata; var Outdata; Size: longword);
      { Decrypt size bytes of data using the CTR method of decryption }

    procedure EncryptECB(const Indata; var Outdata);virtual;
      { Override it! }
    procedure DecryptECB(const Indata; var Outdata);virtual;
      { Override it! }

    destructor Destroy; virtual;
  end;

//Blowfish cipher implementation
const
  PBoxOrg: array[0..17] of DWord= (
    $243f6a88, $85a308d3, $13198a2e, $03707344,
    $a4093822, $299f31d0, $082efa98, $ec4e6c89,
    $452821e6, $38d01377, $be5466cf, $34e90c6c,
    $c0ac29b7, $c97c50dd, $3f84d5b5, $b5470917,
    $9216d5d9, $8979fb1b);
  SBoxOrg: array[0..3,0..255] of DWord= ((
    $d1310ba6, $98dfb5ac, $2ffd72db, $d01adfb7,
    $b8e1afed, $6a267e96, $ba7c9045, $f12c7f99,
    $24a19947, $b3916cf7, $0801f2e2, $858efc16,
    $636920d8, $71574e69, $a458fea3, $f4933d7e,
    $0d95748f, $728eb658, $718bcd58, $82154aee,
    $7b54a41d, $c25a59b5, $9c30d539, $2af26013,
    $c5d1b023, $286085f0, $ca417918, $b8db38ef, 
    $8e79dcb0, $603a180e, $6c9e0e8b, $b01e8a3e,
    $d71577c1, $bd314b27, $78af2fda, $55605c60, 
    $e65525f3, $aa55ab94, $57489862, $63e81440,
    $55ca396a, $2aab10b6, $b4cc5c34, $1141e8ce, 
    $a15486af, $7c72e993, $b3ee1411, $636fbc2a,
    $2ba9c55d, $741831f6, $ce5c3e16, $9b87931e, 
    $afd6ba33, $6c24cf5c, $7a325381, $28958677, 
    $3b8f4898, $6b4bb9af, $c4bfe81b, $66282193, 
    $61d809cc, $fb21a991, $487cac60, $5dec8032, 
    $ef845d5d, $e98575b1, $dc262302, $eb651b88, 
    $23893e81, $d396acc5, $0f6d6ff3, $83f44239,
    $2e0b4482, $a4842004, $69c8f04a, $9e1f9b5e,
    $21c66842, $f6e96c9a, $670c9c61, $abd388f0, 
    $6a51a0d2, $d8542f68, $960fa728, $ab5133a3, 
    $6eef0b6c, $137a3be4, $ba3bf050, $7efb2a98, 
    $a1f1651d, $39af0176, $66ca593e, $82430e88, 
    $8cee8619, $456f9fb4, $7d84a5c3, $3b8b5ebe,
    $e06f75d8, $85c12073, $401a449f, $56c16aa6, 
    $4ed3aa62, $363f7706, $1bfedf72, $429b023d, 
    $37d0d724, $d00a1248, $db0fead3, $49f1c09b, 
    $075372c9, $80991b7b, $25d479d8, $f6e8def7, 
    $e3fe501a, $b6794c3b, $976ce0bd, $04c006ba, 
    $c1a94fb6, $409f60c4, $5e5c9ec2, $196a2463,
    $68fb6faf, $3e6c53b5, $1339b2eb, $3b52ec6f, 
    $6dfc511f, $9b30952c, $cc814544, $af5ebd09, 
    $bee3d004, $de334afd, $660f2807, $192e4bb3,
    $c0cba857, $45c8740f, $d20b5f39, $b9d3fbdb, 
    $5579c0bd, $1a60320a, $d6a100c6, $402c7279, 
    $679f25fe, $fb1fa3cc, $8ea5e9f8, $db3222f8,
    $3c7516df, $fd616b15, $2f501ec8, $ad0552ab, 
    $323db5fa, $fd238760, $53317b48, $3e00df82, 
    $9e5c57bb, $ca6f8ca0, $1a87562e, $df1769db, 
    $d542a8f6, $287effc3, $ac6732c6, $8c4f5573, 
    $695b27b0, $bbca58c8, $e1ffa35d, $b8f011a0, 
    $10fa3d98, $fd2183b8, $4afcb56c, $2dd1d35b,
    $9a53e479, $b6f84565, $d28e49bc, $4bfb9790, 
    $e1ddf2da, $a4cb7e33, $62fb1341, $cee4c6e8, 
    $ef20cada, $36774c01, $d07e9efe, $2bf11fb4, 
    $95dbda4d, $ae909198, $eaad8e71, $6b93d5a0, 
    $d08ed1d0, $afc725e0, $8e3c5b2f, $8e7594b7, 
    $8ff6e2fb, $f2122b64, $8888b812, $900df01c,
    $4fad5ea0, $688fc31c, $d1cff191, $b3a8c1ad, 
    $2f2f2218, $be0e1777, $ea752dfe, $8b021fa1, 
    $e5a0cc0f, $b56f74e8, $18acf3d6, $ce89e299, 
    $b4a84fe0, $fd13e0b7, $7cc43b81, $d2ada8d9, 
    $165fa266, $80957705, $93cc7314, $211a1477, 
    $e6ad2065, $77b5fa86, $c75442f5, $fb9d35cf,
    $ebcdaf0c, $7b3e89a0, $d6411bd3, $ae1e7e49, 
    $00250e2d, $2071b35e, $226800bb, $57b8e0af,
    $2464369b, $f009b91e, $5563911d, $59dfa6aa, 
    $78c14389, $d95a537f, $207d5ba2, $02e5b9c5, 
    $83260376, $6295cfa9, $11c81968, $4e734a41, 
    $b3472dca, $7b14a94a, $1b510052, $9a532915,
    $d60f573f, $bc9bc6e4, $2b60a476, $81e67400,
    $08ba6fb5, $571be91f, $f296ec6b, $2a0dd915, 
    $b6636521, $e7b9f9b6, $ff34052e, $c5855664, 
    $53b02d5d, $a99f8fa1, $08ba4799, $6e85076a),( 
    $4b7a70e9, $b5b32944, $db75092e, $c4192623,
    $ad6ea6b0, $49a7df7d, $9cee60b8, $8fedb266,
    $ecaa8c71, $699a17ff, $5664526c, $c2b19ee1, 
    $193602a5, $75094c29, $a0591340, $e4183a3e, 
    $3f54989a, $5b429d65, $6b8fe4d6, $99f73fd6, 
    $a1d29c07, $efe830f5, $4d2d38e6, $f0255dc1, 
    $4cdd2086, $8470eb26, $6382e9c6, $021ecc5e, 
    $09686b3f, $3ebaefc9, $3c971814, $6b6a70a1,
    $687f3584, $52a0e286, $b79c5305, $aa500737, 
    $3e07841c, $7fdeae5c, $8e7d44ec, $5716f2b8, 
    $b03ada37, $f0500c0d, $f01c1f04, $0200b3ff, 
    $ae0cf51a, $3cb574b2, $25837a58, $dc0921bd,
    $d19113f9, $7ca92ff6, $94324773, $22f54701, 
    $3ae5e581, $37c2dadc, $c8b57634, $9af3dda7,
    $a9446146, $0fd0030e, $ecc8c73e, $a4751e41, 
    $e238cd99, $3bea0e2f, $3280bba1, $183eb331, 
    $4e548b38, $4f6db908, $6f420d03, $f60a04bf, 
    $2cb81290, $24977c79, $5679b072, $bcaf89af, 
    $de9a771f, $d9930810, $b38bae12, $dccf3f2e,
    $5512721f, $2e6b7124, $501adde6, $9f84cd87, 
    $7a584718, $7408da17, $bc9f9abc, $e94b7d8c, 
    $ec7aec3a, $db851dfa, $63094366, $c464c3d2, 
    $ef1c1847, $3215d908, $dd433b37, $24c2ba16, 
    $12a14d43, $2a65c451, $50940002, $133ae4dd,
    $71dff89e, $10314e55, $81ac77d6, $5f11199b,
    $043556f1, $d7a3c76b, $3c11183b, $5924a509, 
    $f28fe6ed, $97f1fbfa, $9ebabf2c, $1e153c6e, 
    $86e34570, $eae96fb1, $860e5e0a, $5a3e2ab3, 
    $771fe71c, $4e3d06fa, $2965dcb9, $99e71d0f,
    $803e89d6, $5266c825, $2e4cc978, $9c10b36a, 
    $c6150eba, $94e2ea78, $a5fc3c53, $1e0a2df4, 
    $f2f74ea7, $361d2b3d, $1939260f, $19c27960,
    $5223a708, $f71312b6, $ebadfe6e, $eac31f66, 
    $e3bc4595, $a67bc883, $b17f37d1, $018cff28,
    $c332ddef, $be6c5aa5, $65582185, $68ab9802, 
    $eecea50f, $db2f953b, $2aef7dad, $5b6e2f84, 
    $1521b628, $29076170, $ecdd4775, $619f1510, 
    $13cca830, $eb61bd96, $0334fe1e, $aa0363cf, 
    $b5735c90, $4c70a239, $d59e9e0b, $cbaade14,
    $eecc86bc, $60622ca7, $9cab5cab, $b2f3846e, 
    $648b1eaf, $19bdf0ca, $a02369b9, $655abb50, 
    $40685a32, $3c2ab4b3, $319ee9d5, $c021b8f7, 
    $9b540b19, $875fa099, $95f7997e, $623d7da8, 
    $f837889a, $97e32d77, $11ed935f, $16681281,
    $0e358829, $c7e61fd6, $96dedfa1, $7858ba99, 
    $57f584a5, $1b227263, $9b83c3ff, $1ac24696, 
    $cdb30aeb, $532e3054, $8fd948e4, $6dbc3128,
    $58ebf2ef, $34c6ffea, $fe28ed61, $ee7c3c73, 
    $5d4a14d9, $e864b7e3, $42105d14, $203e13e0,
    $45eee2b6, $a3aaabea, $db6c4f15, $facb4fd0, 
    $c742f442, $ef6abbb5, $654f3b1d, $41cd2105, 
    $d81e799e, $86854dc7, $e44b476a, $3d816250,
    $cf62a1f2, $5b8d2646, $fc8883a0, $c1c7b6a3,
    $7f1524c3, $69cb7492, $47848a0b, $5692b285,
    $095bbf00, $ad19489d, $1462b174, $23820e00, 
    $58428d2a, $0c55f5ea, $1dadf43e, $233f7061, 
    $3372f092, $8d937e41, $d65fecf1, $6c223bdb, 
    $7cde3759, $cbee7460, $4085f2a7, $ce77326e, 
    $a6078084, $19f8509e, $e8efd855, $61d99735,
    $a969a7aa, $c50c06c2, $5a04abfc, $800bcadc, 
    $9e447a2e, $c3453484, $fdd56705, $0e1e9ec9, 
    $db73dbd3, $105588cd, $675fda79, $e3674340, 
    $c5c43465, $713e38d8, $3d28f89e, $f16dff20, 
    $153e21e7, $8fb03d4a, $e6e39f2b, $db83adf7),(
    $e93d5a68, $948140f7, $f64c261c, $94692934,
    $411520f7, $7602d4f7, $bcf46b2e, $d4a20068, 
    $d4082471, $3320f46a, $43b7d4b7, $500061af, 
    $1e39f62e, $97244546, $14214f74, $bf8b8840, 
    $4d95fc1d, $96b591af, $70f4ddd3, $66a02f45,
    $bfbc09ec, $03bd9785, $7fac6dd0, $31cb8504,
    $96eb27b3, $55fd3941, $da2547e6, $abca0a9a, 
    $28507825, $530429f4, $0a2c86da, $e9b66dfb,
    $68dc1462, $d7486900, $680ec0a4, $27a18dee, 
    $4f3ffea2, $e887ad8c, $b58ce006, $7af4d6b6,
    $aace1e7c, $d3375fec, $ce78a399, $406b2a42, 
    $20fe9e35, $d9f385b9, $ee39d7ab, $3b124e8b, 
    $1dc9faf7, $4b6d1856, $26a36631, $eae397b2, 
    $3a6efa74, $dd5b4332, $6841e7f7, $ca7820fb, 
    $fb0af54e, $d8feb397, $454056ac, $ba489527,
    $55533a3a, $20838d87, $fe6ba9b7, $d096954b, 
    $55a867bc, $a1159a58, $cca92963, $99e1db33,
    $a62a4a56, $3f3125f9, $5ef47e1c, $9029317c, 
    $fdf8e802, $04272f70, $80bb155c, $05282ce3, 
    $95c11548, $e4c66d22, $48c1133f, $c70f86dc,
    $07f9c9ee, $41041f0f, $404779a4, $5d886e17,
    $325f51eb, $d59bc0d1, $f2bcc18f, $41113564,
    $257b7834, $602a9c60, $dff8e8a3, $1f636c1b, 
    $0e12b4c2, $02e1329e, $af664fd1, $cad18115, 
    $6b2395e0, $333e92e1, $3b240b62, $eebeb922,
    $85b2a20e, $e6ba0d99, $de720c8c, $2da2f728, 
    $d0127845, $95b794fd, $647d0862, $e7ccf5f0, 
    $5449a36f, $877d48fa, $c39dfd27, $f33e8d1e,
    $0a476341, $992eff74, $3a6f6eab, $f4f8fd37,
    $a812dc60, $a1ebddf8, $991be14c, $db6e6b0d,
    $c67b5510, $6d672c37, $2765d43b, $dcd0e804, 
    $f1290dc7, $cc00ffa3, $b5390f92, $690fed0b, 
    $667b9ffb, $cedb7d9c, $a091cf0b, $d9155ea3, 
    $bb132f88, $515bad24, $7b9479bf, $763bd6eb, 
    $37392eb3, $cc115979, $8026e297, $f42e312d,
    $6842ada7, $c66a2b3b, $12754ccc, $782ef11c,
    $6a124237, $b79251e7, $06a1bbe6, $4bfb6350, 
    $1a6b1018, $11caedfa, $3d25bdd8, $e2e1c3c9, 
    $44421659, $0a121386, $d90cec6e, $d5abea2a, 
    $64af674e, $da86a85f, $bebfe988, $64e4c3fe,
    $9dbc8057, $f0f7c086, $60787bf8, $6003604d, 
    $d1fd8346, $f6381fb0, $7745ae04, $d736fccc,
    $83426b33, $f01eab71, $b0804187, $3c005e5f, 
    $77a057be, $bde8ae24, $55464299, $bf582e61, 
    $4e58f48f, $f2ddfda2, $f474ef38, $8789bdc2,
    $5366f9c3, $c8b38e74, $b475f255, $46fcd9b9, 
    $7aeb2661, $8b1ddf84, $846a0e79, $915f95e2, 
    $466e598e, $20b45770, $8cd55591, $c902de4c,
    $b90bace1, $bb8205d0, $11a86248, $7574a99e, 
    $b77f19b6, $e0a9dc09, $662d09a1, $c4324633,
    $e85a1f02, $09f0be8c, $4a99a025, $1d6efe10, 
    $1ab93d1d, $0ba5a4df, $a186f20f, $2868f169,
    $dcb7da83, $573906fe, $a1e2ce9b, $4fcd7f52, 
    $50115e01, $a70683fa, $a002b5c4, $0de6d027, 
    $9af88c27, $773f8641, $c3604c06, $61a806b5,
    $f0177a28, $c0f586e0, $006058aa, $30dc7d62, 
    $11e69ed7, $2338ea63, $53c2dd94, $c2c21634, 
    $bbcbee56, $90bcb6de, $ebfc7da1, $ce591d76, 
    $6f05e409, $4b7c0188, $39720a3d, $7c927c24,
    $86e3725f, $724d9db9, $1ac15bb4, $d39eb8fc,
    $ed545578, $08fca5b5, $d83d7cd3, $4dad0fc4, 
    $1e50ef5e, $b161e6f8, $a28514d9, $6c51133c,
    $6fd5c7e7, $56e14ec4, $362abfce, $ddc6c837,
    $d79a3234, $92638212, $670efa8e, $406000e0),( 
    $3a39ce37, $d3faf5cf, $abc27737, $5ac52d1b,
    $5cb0679e, $4fa33742, $d3822740, $99bc9bbe, 
    $d5118e9d, $bf0f7315, $d62d1c7e, $c700c47b,
    $b78c1b6b, $21a19045, $b26eb1be, $6a366eb4,
    $5748ab2f, $bc946e79, $c6a376d2, $6549c2c8, 
    $530ff8ee, $468dde7d, $d5730a1d, $4cd04dc6,
    $2939bbdb, $a9ba4650, $ac9526e8, $be5ee304, 
    $a1fad5f0, $6a2d519a, $63ef8ce2, $9a86ee22, 
    $c089c2b8, $43242ef6, $a51e03aa, $9cf2d0a4,
    $83c061ba, $9be96a4d, $8fe51550, $ba645bd6, 
    $2826a2f9, $a73a3ae1, $4ba99586, $ef5562e9,
    $c72fefd3, $f752f7da, $3f046f69, $77fa0a59, 
    $80e4a915, $87b08601, $9b09e6ad, $3b3ee593, 
    $e990fd5a, $9e34d797, $2cf0b7d9, $022b8b51, 
    $96d5ac3a, $017da67d, $d1cf3ed6, $7c7d2d28, 
    $1f9f25cf, $adf2b89b, $5ad6b472, $5a88f54c,
    $e029ac71, $e019a5e6, $47b0acfd, $ed93fa9b, 
    $e8d3c48d, $283b57cc, $f8d56629, $79132e28,
    $785f0191, $ed756055, $f7960e44, $e3d35e8c, 
    $15056dd4, $88f46dba, $03a16125, $0564f0bd, 
    $c3eb9e15, $3c9057a2, $97271aec, $a93a072a,
    $1b3f6d9b, $1e6321f5, $f59c66fb, $26dcf319, 
    $7533d928, $b155fdf5, $03563482, $8aba3cbb,
    $28517711, $c20ad9f8, $abcc5167, $ccad925f,
    $4de81751, $3830dc8e, $379d5862, $9320f991, 
    $ea7a90c2, $fb3e7bce, $5121ce64, $774fbe32,
    $a8b6e37e, $c3293d46, $48de5369, $6413e680, 
    $a2ae0810, $dd6db224, $69852dfd, $09072166, 
    $b39a460a, $6445c0dd, $586cdecf, $1c20c8ae, 
    $5bbef7dd, $1b588d40, $ccd2017f, $6bb4e3bb, 
    $dda26a7e, $3a59ff45, $3e350a44, $bcb4cdd5,
    $72eacea8, $fa6484bb, $8d6612ae, $bf3c6f47, 
    $d29be463, $542f5d9e, $aec2771b, $f64e6370, 
    $740e0d8d, $e75b1357, $f8721671, $af537d5d,
    $4040cb08, $4eb4e2cc, $34d2466a, $0115af84, 
    $e1b00428, $95983a1d, $06b89fb4, $ce6ea048,
    $6f3f3b82, $3520ab82, $011a1d4b, $277227f8,
    $611560b1, $e7933fdc, $bb3a792b, $344525bd, 
    $a08839e1, $51ce794b, $2f32c9b7, $a01fbac9, 
    $e01cc87e, $bcc7d1f6, $cf0111c3, $a1e8aac7, 
    $1a908749, $d44fbd9a, $d0dadecb, $d50ada38,
    $0339c32a, $c6913667, $8df9317c, $e0b12b4f, 
    $f79e59b7, $43f5bb3a, $f2d519ff, $27d9459c,
    $bf97222c, $15e6fc2a, $0f91fc71, $9b941525,
    $fae59361, $ceb69ceb, $c2a86459, $12baa8d1, 
    $b6c1075e, $e3056a0c, $10d25065, $cb03a442,
    $e0ec6e0e, $1698db3b, $4c98a0be, $3278e964, 
    $9f1f9532, $e0d392df, $d3a0342b, $8971f21e, 
    $1b0a7441, $4ba3348c, $c5be7120, $c37632d8, 
    $df359f8d, $9b992f2e, $e60b6f47, $0fe3f11d, 
    $e54cda54, $1edad891, $ce6279cf, $cd3e7e6f,
    $1618b166, $fd2c1d05, $848fd2c5, $f6fb2299, 
    $f523f357, $a6327623, $93a83531, $56cccd02, 
    $acf08162, $5a75ebb5, $6e163697, $88d273cc, 
    $de966292, $81b949d0, $4c50901b, $71c65614, 
    $e6c6c7bd, $327a140a, $45e1d006, $c3f27b9a,
    $c9aa53fd, $62a80f00, $bb25bfe2, $35bdd2f6,
    $71126905, $b2040222, $b6cbcf7c, $cd769c2b, 
    $53113ec0, $1640e3d3, $38abbd60, $2547adf0, 
    $ba38209c, $f746ce76, $77afa1c5, $20756060, 
    $85cbfe4e, $8ae88dd8, $7aaaf9b0, $4cf9aa7e,
    $1948c25c, $02fb8a8c, $01c36ae4, $d6ebe1f9, 
    $90d4f869, $a65cdea0, $3f09252d, $c208e69f,
    $b74e6132, $ce77e25b, $578fdfe3, $3ac372e6));


type
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;


type
 PBlowfish = ^TBlowfish;
 TBlowfish = object(TBlockCipher64)
  protected
    SBox: array[0..3,0..255] of DWord;
    PBox: array[0..17] of DWord;
  public

    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;


  destructor Destroy; virtual;
 end;

//Cast128 cipher implementation
const
  cast_sbox1: array[0..255]of DWord= (
    $30FB40D4, $9FA0FF0B, $6BECCD2F, $3F258C7A,
    $1E213F2F, $9C004DD3, $6003E540, $CF9FC949,
    $BFD4AF27, $88BBBDB5, $E2034090, $98D09675,
    $6E63A0E0, $15C361D2, $C2E7661D, $22D4FF8E,
    $28683B6F, $C07FD059, $FF2379C8, $775F50E2,
    $43C340D3, $DF2F8656, $887CA41A, $A2D2BD2D,
    $A1C9E0D6, $346C4819, $61B76D87, $22540F2F,
    $2ABE32E1, $AA54166B, $22568E3A, $A2D341D0,
    $66DB40C8, $A784392F, $004DFF2F, $2DB9D2DE,
    $97943FAC, $4A97C1D8, $527644B7, $B5F437A7,
    $B82CBAEF, $D751D159, $6FF7F0ED, $5A097A1F,
    $827B68D0, $90ECF52E, $22B0C054, $BC8E5935,
    $4B6D2F7F, $50BB64A2, $D2664910, $BEE5812D,
    $B7332290, $E93B159F, $B48EE411, $4BFF345D,
    $FD45C240, $AD31973F, $C4F6D02E, $55FC8165,
    $D5B1CAAD, $A1AC2DAE, $A2D4B76D, $C19B0C50,
    $882240F2, $0C6E4F38, $A4E4BFD7, $4F5BA272,
    $564C1D2F, $C59C5319, $B949E354, $B04669FE,
    $B1B6AB8A, $C71358DD, $6385C545, $110F935D,
    $57538AD5, $6A390493, $E63D37E0, $2A54F6B3,
    $3A787D5F, $6276A0B5, $19A6FCDF, $7A42206A,
    $29F9D4D5, $F61B1891, $BB72275E, $AA508167,
    $38901091, $C6B505EB, $84C7CB8C, $2AD75A0F,
    $874A1427, $A2D1936B, $2AD286AF, $AA56D291,
    $D7894360, $425C750D, $93B39E26, $187184C9,
    $6C00B32D, $73E2BB14, $A0BEBC3C, $54623779,
    $64459EAB, $3F328B82, $7718CF82, $59A2CEA6,
    $04EE002E, $89FE78E6, $3FAB0950, $325FF6C2,
    $81383F05, $6963C5C8, $76CB5AD6, $D49974C9,
    $CA180DCF, $380782D5, $C7FA5CF6, $8AC31511,
    $35E79E13, $47DA91D0, $F40F9086, $A7E2419E,
    $31366241, $051EF495, $AA573B04, $4A805D8D,
    $548300D0, $00322A3C, $BF64CDDF, $BA57A68E,
    $75C6372B, $50AFD341, $A7C13275, $915A0BF5,
    $6B54BFAB, $2B0B1426, $AB4CC9D7, $449CCD82,
    $F7FBF265, $AB85C5F3, $1B55DB94, $AAD4E324,
    $CFA4BD3F, $2DEAA3E2, $9E204D02, $C8BD25AC,
    $EADF55B3, $D5BD9E98, $E31231B2, $2AD5AD6C,
    $954329DE, $ADBE4528, $D8710F69, $AA51C90F,
    $AA786BF6, $22513F1E, $AA51A79B, $2AD344CC,
    $7B5A41F0, $D37CFBAD, $1B069505, $41ECE491,
    $B4C332E6, $032268D4, $C9600ACC, $CE387E6D,
    $BF6BB16C, $6A70FB78, $0D03D9C9, $D4DF39DE,
    $E01063DA, $4736F464, $5AD328D8, $B347CC96,
    $75BB0FC3, $98511BFB, $4FFBCC35, $B58BCF6A,
    $E11F0ABC, $BFC5FE4A, $A70AEC10, $AC39570A,
    $3F04442F, $6188B153, $E0397A2E, $5727CB79,
    $9CEB418F, $1CACD68D, $2AD37C96, $0175CB9D,
    $C69DFF09, $C75B65F0, $D9DB40D8, $EC0E7779,
    $4744EAD4, $B11C3274, $DD24CB9E, $7E1C54BD,
    $F01144F9, $D2240EB1, $9675B3FD, $A3AC3755,
    $D47C27AF, $51C85F4D, $56907596, $A5BB15E6,
    $580304F0, $CA042CF1, $011A37EA, $8DBFAADB,
    $35BA3E4A, $3526FFA0, $C37B4D09, $BC306ED9,
    $98A52666, $5648F725, $FF5E569D, $0CED63D0,
    $7C63B2CF, $700B45E1, $D5EA50F1, $85A92872,
    $AF1FBDA7, $D4234870, $A7870BF3, $2D3B4D79,
    $42E04198, $0CD0EDE7, $26470DB8, $F881814C,
    $474D6AD7, $7C0C5E5C, $D1231959, $381B7298,
    $F5D2F4DB, $AB838653, $6E2F1E23, $83719C9E,
    $BD91E046, $9A56456E, $DC39200C, $20C8C571,
    $962BDA1C, $E1E696FF, $B141AB08, $7CCA89B9,
    $1A69E783, $02CC4843, $A2F7C579, $429EF47D,
    $427B169C, $5AC9F049, $DD8F0F00, $5C8165BF
   );

   cast_sbox2: array[0..255] of DWord = (
    $1F201094, $EF0BA75B, $69E3CF7E, $393F4380,
    $FE61CF7A, $EEC5207A, $55889C94, $72FC0651,
    $ADA7EF79, $4E1D7235, $D55A63CE, $DE0436BA,
    $99C430EF, $5F0C0794, $18DCDB7D, $A1D6EFF3,
    $A0B52F7B, $59E83605, $EE15B094, $E9FFD909,
    $DC440086, $EF944459, $BA83CCB3, $E0C3CDFB,
    $D1DA4181, $3B092AB1, $F997F1C1, $A5E6CF7B,
    $01420DDB, $E4E7EF5B, $25A1FF41, $E180F806,
    $1FC41080, $179BEE7A, $D37AC6A9, $FE5830A4,
    $98DE8B7F, $77E83F4E, $79929269, $24FA9F7B,
    $E113C85B, $ACC40083, $D7503525, $F7EA615F,
    $62143154, $0D554B63, $5D681121, $C866C359,
    $3D63CF73, $CEE234C0, $D4D87E87, $5C672B21,
    $071F6181, $39F7627F, $361E3084, $E4EB573B,
    $602F64A4, $D63ACD9C, $1BBC4635, $9E81032D,
    $2701F50C, $99847AB4, $A0E3DF79, $BA6CF38C,
    $10843094, $2537A95E, $F46F6FFE, $A1FF3B1F,
    $208CFB6A, $8F458C74, $D9E0A227, $4EC73A34,
    $FC884F69, $3E4DE8DF, $EF0E0088, $3559648D,
    $8A45388C, $1D804366, $721D9BFD, $A58684BB,
    $E8256333, $844E8212, $128D8098, $FED33FB4,
    $CE280AE1, $27E19BA5, $D5A6C252, $E49754BD,
    $C5D655DD, $EB667064, $77840B4D, $A1B6A801,
    $84DB26A9, $E0B56714, $21F043B7, $E5D05860,
    $54F03084, $066FF472, $A31AA153, $DADC4755,
    $B5625DBF, $68561BE6, $83CA6B94, $2D6ED23B,
    $ECCF01DB, $A6D3D0BA, $B6803D5C, $AF77A709,
    $33B4A34C, $397BC8D6, $5EE22B95, $5F0E5304,
    $81ED6F61, $20E74364, $B45E1378, $DE18639B,
    $881CA122, $B96726D1, $8049A7E8, $22B7DA7B,
    $5E552D25, $5272D237, $79D2951C, $C60D894C,
    $488CB402, $1BA4FE5B, $A4B09F6B, $1CA815CF,
    $A20C3005, $8871DF63, $B9DE2FCB, $0CC6C9E9,
    $0BEEFF53, $E3214517, $B4542835, $9F63293C,
    $EE41E729, $6E1D2D7C, $50045286, $1E6685F3,
    $F33401C6, $30A22C95, $31A70850, $60930F13,
    $73F98417, $A1269859, $EC645C44, $52C877A9,
    $CDFF33A6, $A02B1741, $7CBAD9A2, $2180036F,
    $50D99C08, $CB3F4861, $C26BD765, $64A3F6AB,
    $80342676, $25A75E7B, $E4E6D1FC, $20C710E6,
    $CDF0B680, $17844D3B, $31EEF84D, $7E0824E4,
    $2CCB49EB, $846A3BAE, $8FF77888, $EE5D60F6,
    $7AF75673, $2FDD5CDB, $A11631C1, $30F66F43,
    $B3FAEC54, $157FD7FA, $EF8579CC, $D152DE58,
    $DB2FFD5E, $8F32CE19, $306AF97A, $02F03EF8,
    $99319AD5, $C242FA0F, $A7E3EBB0, $C68E4906,
    $B8DA230C, $80823028, $DCDEF3C8, $D35FB171,
    $088A1BC8, $BEC0C560, $61A3C9E8, $BCA8F54D,
    $C72FEFFA, $22822E99, $82C570B4, $D8D94E89,
    $8B1C34BC, $301E16E6, $273BE979, $B0FFEAA6,
    $61D9B8C6, $00B24869, $B7FFCE3F, $08DC283B,
    $43DAF65A, $F7E19798, $7619B72F, $8F1C9BA4,
    $DC8637A0, $16A7D3B1, $9FC393B7, $A7136EEB,
    $C6BCC63E, $1A513742, $EF6828BC, $520365D6,
    $2D6A77AB, $3527ED4B, $821FD216, $095C6E2E,
    $DB92F2FB, $5EEA29CB, $145892F5, $91584F7F,
    $5483697B, $2667A8CC, $85196048, $8C4BACEA,
    $833860D4, $0D23E0F9, $6C387E8A, $0AE6D249,
    $B284600C, $D835731D, $DCB1C647, $AC4C56EA,
    $3EBD81B3, $230EABB0, $6438BC87, $F0B5B1FA,
    $8F5EA2B3, $FC184642, $0A036B7A, $4FB089BD,
    $649DA589, $A345415E, $5C038323, $3E5D3BB9,
    $43D79572, $7E6DD07C, $06DFDF1E, $6C6CC4EF,
    $7160A539, $73BFBE70, $83877605, $4523ECF1
   );

   cast_sbox3: array[0..255] of DWord = (
    $8DEFC240, $25FA5D9F, $EB903DBF, $E810C907,
    $47607FFF, $369FE44B, $8C1FC644, $AECECA90,
    $BEB1F9BF, $EEFBCAEA, $E8CF1950, $51DF07AE,
    $920E8806, $F0AD0548, $E13C8D83, $927010D5,
    $11107D9F, $07647DB9, $B2E3E4D4, $3D4F285E,
    $B9AFA820, $FADE82E0, $A067268B, $8272792E,
    $553FB2C0, $489AE22B, $D4EF9794, $125E3FBC,
    $21FFFCEE, $825B1BFD, $9255C5ED, $1257A240,
    $4E1A8302, $BAE07FFF, $528246E7, $8E57140E,
    $3373F7BF, $8C9F8188, $A6FC4EE8, $C982B5A5,
    $A8C01DB7, $579FC264, $67094F31, $F2BD3F5F,
    $40FFF7C1, $1FB78DFC, $8E6BD2C1, $437BE59B,
    $99B03DBF, $B5DBC64B, $638DC0E6, $55819D99,
    $A197C81C, $4A012D6E, $C5884A28, $CCC36F71,
    $B843C213, $6C0743F1, $8309893C, $0FEDDD5F,
    $2F7FE850, $D7C07F7E, $02507FBF, $5AFB9A04,
    $A747D2D0, $1651192E, $AF70BF3E, $58C31380,
    $5F98302E, $727CC3C4, $0A0FB402, $0F7FEF82,
    $8C96FDAD, $5D2C2AAE, $8EE99A49, $50DA88B8,
    $8427F4A0, $1EAC5790, $796FB449, $8252DC15,
    $EFBD7D9B, $A672597D, $ADA840D8, $45F54504,
    $FA5D7403, $E83EC305, $4F91751A, $925669C2,
    $23EFE941, $A903F12E, $60270DF2, $0276E4B6,
    $94FD6574, $927985B2, $8276DBCB, $02778176,
    $F8AF918D, $4E48F79E, $8F616DDF, $E29D840E,
    $842F7D83, $340CE5C8, $96BBB682, $93B4B148,
    $EF303CAB, $984FAF28, $779FAF9B, $92DC560D,
    $224D1E20, $8437AA88, $7D29DC96, $2756D3DC,
    $8B907CEE, $B51FD240, $E7C07CE3, $E566B4A1,
    $C3E9615E, $3CF8209D, $6094D1E3, $CD9CA341,
    $5C76460E, $00EA983B, $D4D67881, $FD47572C,
    $F76CEDD9, $BDA8229C, $127DADAA, $438A074E,
    $1F97C090, $081BDB8A, $93A07EBE, $B938CA15,
    $97B03CFF, $3DC2C0F8, $8D1AB2EC, $64380E51,
    $68CC7BFB, $D90F2788, $12490181, $5DE5FFD4,
    $DD7EF86A, $76A2E214, $B9A40368, $925D958F,
    $4B39FFFA, $BA39AEE9, $A4FFD30B, $FAF7933B,
    $6D498623, $193CBCFA, $27627545, $825CF47A,
    $61BD8BA0, $D11E42D1, $CEAD04F4, $127EA392,
    $10428DB7, $8272A972, $9270C4A8, $127DE50B,
    $285BA1C8, $3C62F44F, $35C0EAA5, $E805D231,
    $428929FB, $B4FCDF82, $4FB66A53, $0E7DC15B,
    $1F081FAB, $108618AE, $FCFD086D, $F9FF2889,
    $694BCC11, $236A5CAE, $12DECA4D, $2C3F8CC5,
    $D2D02DFE, $F8EF5896, $E4CF52DA, $95155B67,
    $494A488C, $B9B6A80C, $5C8F82BC, $89D36B45,
    $3A609437, $EC00C9A9, $44715253, $0A874B49,
    $D773BC40, $7C34671C, $02717EF6, $4FEB5536,
    $A2D02FFF, $D2BF60C4, $D43F03C0, $50B4EF6D,
    $07478CD1, $006E1888, $A2E53F55, $B9E6D4BC,
    $A2048016, $97573833, $D7207D67, $DE0F8F3D,
    $72F87B33, $ABCC4F33, $7688C55D, $7B00A6B0,
    $947B0001, $570075D2, $F9BB88F8, $8942019E,
    $4264A5FF, $856302E0, $72DBD92B, $EE971B69,
    $6EA22FDE, $5F08AE2B, $AF7A616D, $E5C98767,
    $CF1FEBD2, $61EFC8C2, $F1AC2571, $CC8239C2,
    $67214CB8, $B1E583D1, $B7DC3E62, $7F10BDCE,
    $F90A5C38, $0FF0443D, $606E6DC6, $60543A49,
    $5727C148, $2BE98A1D, $8AB41738, $20E1BE24,
    $AF96DA0F, $68458425, $99833BE5, $600D457D,
    $282F9350, $8334B362, $D91D1120, $2B6D8DA0,
    $642B1E31, $9C305A00, $52BCE688, $1B03588A,
    $F7BAEFD5, $4142ED9C, $A4315C11, $83323EC5,
    $DFEF4636, $A133C501, $E9D3531C, $EE353783
   );

   cast_sbox4: array[0..255] of DWord = (
    $9DB30420, $1FB6E9DE, $A7BE7BEF, $D273A298,
    $4A4F7BDB, $64AD8C57, $85510443, $FA020ED1,
    $7E287AFF, $E60FB663, $095F35A1, $79EBF120,
    $FD059D43, $6497B7B1, $F3641F63, $241E4ADF,
    $28147F5F, $4FA2B8CD, $C9430040, $0CC32220,
    $FDD30B30, $C0A5374F, $1D2D00D9, $24147B15,
    $EE4D111A, $0FCA5167, $71FF904C, $2D195FFE,
    $1A05645F, $0C13FEFE, $081B08CA, $05170121,
    $80530100, $E83E5EFE, $AC9AF4F8, $7FE72701,
    $D2B8EE5F, $06DF4261, $BB9E9B8A, $7293EA25,
    $CE84FFDF, $F5718801, $3DD64B04, $A26F263B,
    $7ED48400, $547EEBE6, $446D4CA0, $6CF3D6F5,
    $2649ABDF, $AEA0C7F5, $36338CC1, $503F7E93,
    $D3772061, $11B638E1, $72500E03, $F80EB2BB,
    $ABE0502E, $EC8D77DE, $57971E81, $E14F6746,
    $C9335400, $6920318F, $081DBB99, $FFC304A5,
    $4D351805, $7F3D5CE3, $A6C866C6, $5D5BCCA9,
    $DAEC6FEA, $9F926F91, $9F46222F, $3991467D,
    $A5BF6D8E, $1143C44F, $43958302, $D0214EEB,
    $022083B8, $3FB6180C, $18F8931E, $281658E6,
    $26486E3E, $8BD78A70, $7477E4C1, $B506E07C,
    $F32D0A25, $79098B02, $E4EABB81, $28123B23,
    $69DEAD38, $1574CA16, $DF871B62, $211C40B7,
    $A51A9EF9, $0014377B, $041E8AC8, $09114003,
    $BD59E4D2, $E3D156D5, $4FE876D5, $2F91A340,
    $557BE8DE, $00EAE4A7, $0CE5C2EC, $4DB4BBA6,
    $E756BDFF, $DD3369AC, $EC17B035, $06572327,
    $99AFC8B0, $56C8C391, $6B65811C, $5E146119,
    $6E85CB75, $BE07C002, $C2325577, $893FF4EC,
    $5BBFC92D, $D0EC3B25, $B7801AB7, $8D6D3B24,
    $20C763EF, $C366A5FC, $9C382880, $0ACE3205,
    $AAC9548A, $ECA1D7C7, $041AFA32, $1D16625A,
    $6701902C, $9B757A54, $31D477F7, $9126B031,
    $36CC6FDB, $C70B8B46, $D9E66A48, $56E55A79,
    $026A4CEB, $52437EFF, $2F8F76B4, $0DF980A5,
    $8674CDE3, $EDDA04EB, $17A9BE04, $2C18F4DF,
    $B7747F9D, $AB2AF7B4, $EFC34D20, $2E096B7C,
    $1741A254, $E5B6A035, $213D42F6, $2C1C7C26,
    $61C2F50F, $6552DAF9, $D2C231F8, $25130F69,
    $D8167FA2, $0418F2C8, $001A96A6, $0D1526AB,
    $63315C21, $5E0A72EC, $49BAFEFD, $187908D9,
    $8D0DBD86, $311170A7, $3E9B640C, $CC3E10D7,
    $D5CAD3B6, $0CAEC388, $F73001E1, $6C728AFF,
    $71EAE2A1, $1F9AF36E, $CFCBD12F, $C1DE8417,
    $AC07BE6B, $CB44A1D8, $8B9B0F56, $013988C3,
    $B1C52FCA, $B4BE31CD, $D8782806, $12A3A4E2,
    $6F7DE532, $58FD7EB6, $D01EE900, $24ADFFC2,
    $F4990FC5, $9711AAC5, $001D7B95, $82E5E7D2,
    $109873F6, $00613096, $C32D9521, $ADA121FF,
    $29908415, $7FBB977F, $AF9EB3DB, $29C9ED2A,
    $5CE2A465, $A730F32C, $D0AA3FE8, $8A5CC091,
    $D49E2CE7, $0CE454A9, $D60ACD86, $015F1919,
    $77079103, $DEA03AF6, $78A8565E, $DEE356DF,
    $21F05CBE, $8B75E387, $B3C50651, $B8A5C3EF,
    $D8EEB6D2, $E523BE77, $C2154529, $2F69EFDF,
    $AFE67AFB, $F470C4B2, $F3E0EB5B, $D6CC9876,
    $39E4460C, $1FDA8538, $1987832F, $CA007367,
    $A99144F8, $296B299E, $492FC295, $9266BEAB,
    $B5676E69, $9BD3DDDA, $DF7E052F, $DB25701C,
    $1B5E51EE, $F65324E6, $6AFCE36C, $0316CC04,
    $8644213E, $B7DC59D0, $7965291F, $CCD6FD43,
    $41823979, $932BCDF6, $B657C34D, $4EDFD282,
    $7AE5290C, $3CB9536B, $851E20FE, $9833557E,
    $13ECF0B0, $D3FFB372, $3F85C5C1, $0AEF7ED2
   );

   cast_sbox5: array[0..255] of DWord = (
    $7EC90C04, $2C6E74B9, $9B0E66DF, $A6337911,
    $B86A7FFF, $1DD358F5, $44DD9D44, $1731167F,
    $08FBF1FA, $E7F511CC, $D2051B00, $735ABA00,
    $2AB722D8, $386381CB, $ACF6243A, $69BEFD7A,
    $E6A2E77F, $F0C720CD, $C4494816, $CCF5C180,
    $38851640, $15B0A848, $E68B18CB, $4CAADEFF,
    $5F480A01, $0412B2AA, $259814FC, $41D0EFE2,
    $4E40B48D, $248EB6FB, $8DBA1CFE, $41A99B02,
    $1A550A04, $BA8F65CB, $7251F4E7, $95A51725,
    $C106ECD7, $97A5980A, $C539B9AA, $4D79FE6A,
    $F2F3F763, $68AF8040, $ED0C9E56, $11B4958B,
    $E1EB5A88, $8709E6B0, $D7E07156, $4E29FEA7,
    $6366E52D, $02D1C000, $C4AC8E05, $9377F571,
    $0C05372A, $578535F2, $2261BE02, $D642A0C9,
    $DF13A280, $74B55BD2, $682199C0, $D421E5EC,
    $53FB3CE8, $C8ADEDB3, $28A87FC9, $3D959981,
    $5C1FF900, $FE38D399, $0C4EFF0B, $062407EA,
    $AA2F4FB1, $4FB96976, $90C79505, $B0A8A774,
    $EF55A1FF, $E59CA2C2, $A6B62D27, $E66A4263,
    $DF65001F, $0EC50966, $DFDD55BC, $29DE0655,
    $911E739A, $17AF8975, $32C7911C, $89F89468,
    $0D01E980, $524755F4, $03B63CC9, $0CC844B2,
    $BCF3F0AA, $87AC36E9, $E53A7426, $01B3D82B,
    $1A9E7449, $64EE2D7E, $CDDBB1DA, $01C94910,
    $B868BF80, $0D26F3FD, $9342EDE7, $04A5C284,
    $636737B6, $50F5B616, $F24766E3, $8ECA36C1,
    $136E05DB, $FEF18391, $FB887A37, $D6E7F7D4,
    $C7FB7DC9, $3063FCDF, $B6F589DE, $EC2941DA,
    $26E46695, $B7566419, $F654EFC5, $D08D58B7,
    $48925401, $C1BACB7F, $E5FF550F, $B6083049,
    $5BB5D0E8, $87D72E5A, $AB6A6EE1, $223A66CE,
    $C62BF3CD, $9E0885F9, $68CB3E47, $086C010F,
    $A21DE820, $D18B69DE, $F3F65777, $FA02C3F6,
    $407EDAC3, $CBB3D550, $1793084D, $B0D70EBA,
    $0AB378D5, $D951FB0C, $DED7DA56, $4124BBE4,
    $94CA0B56, $0F5755D1, $E0E1E56E, $6184B5BE,
    $580A249F, $94F74BC0, $E327888E, $9F7B5561,
    $C3DC0280, $05687715, $646C6BD7, $44904DB3,
    $66B4F0A3, $C0F1648A, $697ED5AF, $49E92FF6,
    $309E374F, $2CB6356A, $85808573, $4991F840,
    $76F0AE02, $083BE84D, $28421C9A, $44489406,
    $736E4CB8, $C1092910, $8BC95FC6, $7D869CF4,
    $134F616F, $2E77118D, $B31B2BE1, $AA90B472,
    $3CA5D717, $7D161BBA, $9CAD9010, $AF462BA2,
    $9FE459D2, $45D34559, $D9F2DA13, $DBC65487,
    $F3E4F94E, $176D486F, $097C13EA, $631DA5C7,
    $445F7382, $175683F4, $CDC66A97, $70BE0288,
    $B3CDCF72, $6E5DD2F3, $20936079, $459B80A5,
    $BE60E2DB, $A9C23101, $EBA5315C, $224E42F2,
    $1C5C1572, $F6721B2C, $1AD2FFF3, $8C25404E,
    $324ED72F, $4067B7FD, $0523138E, $5CA3BC78,
    $DC0FD66E, $75922283, $784D6B17, $58EBB16E,
    $44094F85, $3F481D87, $FCFEAE7B, $77B5FF76,
    $8C2302BF, $AAF47556, $5F46B02A, $2B092801,
    $3D38F5F7, $0CA81F36, $52AF4A8A, $66D5E7C0,
    $DF3B0874, $95055110, $1B5AD7A8, $F61ED5AD,
    $6CF6E479, $20758184, $D0CEFA65, $88F7BE58,
    $4A046826, $0FF6F8F3, $A09C7F70, $5346ABA0,
    $5CE96C28, $E176EDA3, $6BAC307F, $376829D2,
    $85360FA9, $17E3FE2A, $24B79767, $F5A96B20,
    $D6CD2595, $68FF1EBF, $7555442C, $F19F06BE,
    $F9E0659A, $EEB9491D, $34010718, $BB30CAB8,
    $E822FE15, $88570983, $750E6249, $DA627E55,
    $5E76FFA8, $B1534546, $6D47DE08, $EFE9E7D4
   );

   cast_sbox6: array[0..255] of DWord = (
    $F6FA8F9D, $2CAC6CE1, $4CA34867, $E2337F7C,
    $95DB08E7, $016843B4, $ECED5CBC, $325553AC,
    $BF9F0960, $DFA1E2ED, $83F0579D, $63ED86B9,
    $1AB6A6B8, $DE5EBE39, $F38FF732, $8989B138,
    $33F14961, $C01937BD, $F506C6DA, $E4625E7E,
    $A308EA99, $4E23E33C, $79CBD7CC, $48A14367,
    $A3149619, $FEC94BD5, $A114174A, $EAA01866,
    $A084DB2D, $09A8486F, $A888614A, $2900AF98,
    $01665991, $E1992863, $C8F30C60, $2E78EF3C,
    $D0D51932, $CF0FEC14, $F7CA07D2, $D0A82072,
    $FD41197E, $9305A6B0, $E86BE3DA, $74BED3CD,
    $372DA53C, $4C7F4448, $DAB5D440, $6DBA0EC3,
    $083919A7, $9FBAEED9, $49DBCFB0, $4E670C53,
    $5C3D9C01, $64BDB941, $2C0E636A, $BA7DD9CD,
    $EA6F7388, $E70BC762, $35F29ADB, $5C4CDD8D,
    $F0D48D8C, $B88153E2, $08A19866, $1AE2EAC8,
    $284CAF89, $AA928223, $9334BE53, $3B3A21BF,
    $16434BE3, $9AEA3906, $EFE8C36E, $F890CDD9,
    $80226DAE, $C340A4A3, $DF7E9C09, $A694A807,
    $5B7C5ECC, $221DB3A6, $9A69A02F, $68818A54,
    $CEB2296F, $53C0843A, $FE893655, $25BFE68A,
    $B4628ABC, $CF222EBF, $25AC6F48, $A9A99387,
    $53BDDB65, $E76FFBE7, $E967FD78, $0BA93563,
    $8E342BC1, $E8A11BE9, $4980740D, $C8087DFC,
    $8DE4BF99, $A11101A0, $7FD37975, $DA5A26C0,
    $E81F994F, $9528CD89, $FD339FED, $B87834BF,
    $5F04456D, $22258698, $C9C4C83B, $2DC156BE,
    $4F628DAA, $57F55EC5, $E2220ABE, $D2916EBF,
    $4EC75B95, $24F2C3C0, $42D15D99, $CD0D7FA0,
    $7B6E27FF, $A8DC8AF0, $7345C106, $F41E232F,
    $35162386, $E6EA8926, $3333B094, $157EC6F2,
    $372B74AF, $692573E4, $E9A9D848, $F3160289,
    $3A62EF1D, $A787E238, $F3A5F676, $74364853,
    $20951063, $4576698D, $B6FAD407, $592AF950,
    $36F73523, $4CFB6E87, $7DA4CEC0, $6C152DAA,
    $CB0396A8, $C50DFE5D, $FCD707AB, $0921C42F,
    $89DFF0BB, $5FE2BE78, $448F4F33, $754613C9,
    $2B05D08D, $48B9D585, $DC049441, $C8098F9B,
    $7DEDE786, $C39A3373, $42410005, $6A091751,
    $0EF3C8A6, $890072D6, $28207682, $A9A9F7BE,
    $BF32679D, $D45B5B75, $B353FD00, $CBB0E358,
    $830F220A, $1F8FB214, $D372CF08, $CC3C4A13,
    $8CF63166, $061C87BE, $88C98F88, $6062E397,
    $47CF8E7A, $B6C85283, $3CC2ACFB, $3FC06976,
    $4E8F0252, $64D8314D, $DA3870E3, $1E665459,
    $C10908F0, $513021A5, $6C5B68B7, $822F8AA0,
    $3007CD3E, $74719EEF, $DC872681, $073340D4,
    $7E432FD9, $0C5EC241, $8809286C, $F592D891,
    $08A930F6, $957EF305, $B7FBFFBD, $C266E96F,
    $6FE4AC98, $B173ECC0, $BC60B42A, $953498DA,
    $FBA1AE12, $2D4BD736, $0F25FAAB, $A4F3FCEB,
    $E2969123, $257F0C3D, $9348AF49, $361400BC,
    $E8816F4A, $3814F200, $A3F94043, $9C7A54C2,
    $BC704F57, $DA41E7F9, $C25AD33A, $54F4A084,
    $B17F5505, $59357CBE, $EDBD15C8, $7F97C5AB,
    $BA5AC7B5, $B6F6DEAF, $3A479C3A, $5302DA25,
    $653D7E6A, $54268D49, $51A477EA, $5017D55B,
    $D7D25D88, $44136C76, $0404A8C8, $B8E5A121,
    $B81A928A, $60ED5869, $97C55B96, $EAEC991B,
    $29935913, $01FDB7F1, $088E8DFA, $9AB6F6F5,
    $3B4CBF9F, $4A5DE3AB, $E6051D35, $A0E1D855,
    $D36B4CF1, $F544EDEB, $B0E93524, $BEBB8FBD,
    $A2D762CF, $49C92F54, $38B5F331, $7128A454,
    $48392905, $A65B1DB8, $851C97BD, $D675CF2F
   );

   cast_sbox7: array[0..255] of DWord = (
    $85E04019, $332BF567, $662DBFFF, $CFC65693,
    $2A8D7F6F, $AB9BC912, $DE6008A1, $2028DA1F,
    $0227BCE7, $4D642916, $18FAC300, $50F18B82,
    $2CB2CB11, $B232E75C, $4B3695F2, $B28707DE,
    $A05FBCF6, $CD4181E9, $E150210C, $E24EF1BD,
    $B168C381, $FDE4E789, $5C79B0D8, $1E8BFD43,
    $4D495001, $38BE4341, $913CEE1D, $92A79C3F,
    $089766BE, $BAEEADF4, $1286BECF, $B6EACB19,
    $2660C200, $7565BDE4, $64241F7A, $8248DCA9,
    $C3B3AD66, $28136086, $0BD8DFA8, $356D1CF2,
    $107789BE, $B3B2E9CE, $0502AA8F, $0BC0351E,
    $166BF52A, $EB12FF82, $E3486911, $D34D7516,
    $4E7B3AFF, $5F43671B, $9CF6E037, $4981AC83,
    $334266CE, $8C9341B7, $D0D854C0, $CB3A6C88,
    $47BC2829, $4725BA37, $A66AD22B, $7AD61F1E,
    $0C5CBAFA, $4437F107, $B6E79962, $42D2D816,
    $0A961288, $E1A5C06E, $13749E67, $72FC081A,
    $B1D139F7, $F9583745, $CF19DF58, $BEC3F756,
    $C06EBA30, $07211B24, $45C28829, $C95E317F,
    $BC8EC511, $38BC46E9, $C6E6FA14, $BAE8584A,
    $AD4EBC46, $468F508B, $7829435F, $F124183B,
    $821DBA9F, $AFF60FF4, $EA2C4E6D, $16E39264,
    $92544A8B, $009B4FC3, $ABA68CED, $9AC96F78,
    $06A5B79A, $B2856E6E, $1AEC3CA9, $BE838688,
    $0E0804E9, $55F1BE56, $E7E5363B, $B3A1F25D,
    $F7DEBB85, $61FE033C, $16746233, $3C034C28,
    $DA6D0C74, $79AAC56C, $3CE4E1AD, $51F0C802,
    $98F8F35A, $1626A49F, $EED82B29, $1D382FE3,
    $0C4FB99A, $BB325778, $3EC6D97B, $6E77A6A9,
    $CB658B5C, $D45230C7, $2BD1408B, $60C03EB7,
    $B9068D78, $A33754F4, $F430C87D, $C8A71302,
    $B96D8C32, $EBD4E7BE, $BE8B9D2D, $7979FB06,
    $E7225308, $8B75CF77, $11EF8DA4, $E083C858,
    $8D6B786F, $5A6317A6, $FA5CF7A0, $5DDA0033,
    $F28EBFB0, $F5B9C310, $A0EAC280, $08B9767A,
    $A3D9D2B0, $79D34217, $021A718D, $9AC6336A,
    $2711FD60, $438050E3, $069908A8, $3D7FEDC4,
    $826D2BEF, $4EEB8476, $488DCF25, $36C9D566,
    $28E74E41, $C2610ACA, $3D49A9CF, $BAE3B9DF,
    $B65F8DE6, $92AEAF64, $3AC7D5E6, $9EA80509,
    $F22B017D, $A4173F70, $DD1E16C3, $15E0D7F9,
    $50B1B887, $2B9F4FD5, $625ABA82, $6A017962,
    $2EC01B9C, $15488AA9, $D716E740, $40055A2C,
    $93D29A22, $E32DBF9A, $058745B9, $3453DC1E,
    $D699296E, $496CFF6F, $1C9F4986, $DFE2ED07,
    $B87242D1, $19DE7EAE, $053E561A, $15AD6F8C,
    $66626C1C, $7154C24C, $EA082B2A, $93EB2939,
    $17DCB0F0, $58D4F2AE, $9EA294FB, $52CF564C,
    $9883FE66, $2EC40581, $763953C3, $01D6692E,
    $D3A0C108, $A1E7160E, $E4F2DFA6, $693ED285,
    $74904698, $4C2B0EDD, $4F757656, $5D393378,
    $A132234F, $3D321C5D, $C3F5E194, $4B269301,
    $C79F022F, $3C997E7E, $5E4F9504, $3FFAFBBD,
    $76F7AD0E, $296693F4, $3D1FCE6F, $C61E45BE,
    $D3B5AB34, $F72BF9B7, $1B0434C0, $4E72B567,
    $5592A33D, $B5229301, $CFD2A87F, $60AEB767,
    $1814386B, $30BCC33D, $38A0C07D, $FD1606F2,
    $C363519B, $589DD390, $5479F8E6, $1CB8D647,
    $97FD61A9, $EA7759F4, $2D57539D, $569A58CF,
    $E84E63AD, $462E1B78, $6580F87E, $F3817914,
    $91DA55F4, $40A230F3, $D1988F35, $B6E318D2,
    $3FFA50BC, $3D40F021, $C3C0BDAE, $4958C24C,
    $518F36B2, $84B1D370, $0FEDCE83, $878DDADA,
    $F2A279C7, $94E01BE8, $90716F4B, $954B8AA3
   );

  cast_sbox8: array[0..255] of DWord = (
    $E216300D, $BBDDFFFC, $A7EBDABD, $35648095,
    $7789F8B7, $E6C1121B, $0E241600, $052CE8B5,
    $11A9CFB0, $E5952F11, $ECE7990A, $9386D174,
    $2A42931C, $76E38111, $B12DEF3A, $37DDDDFC,
    $DE9ADEB1, $0A0CC32C, $BE197029, $84A00940,
    $BB243A0F, $B4D137CF, $B44E79F0, $049EEDFD,
    $0B15A15D, $480D3168, $8BBBDE5A, $669DED42,
    $C7ECE831, $3F8F95E7, $72DF191B, $7580330D,
    $94074251, $5C7DCDFA, $ABBE6D63, $AA402164,
    $B301D40A, $02E7D1CA, $53571DAE, $7A3182A2,
    $12A8DDEC, $FDAA335D, $176F43E8, $71FB46D4,
    $38129022, $CE949AD4, $B84769AD, $965BD862,
    $82F3D055, $66FB9767, $15B80B4E, $1D5B47A0,
    $4CFDE06F, $C28EC4B8, $57E8726E, $647A78FC,
    $99865D44, $608BD593, $6C200E03, $39DC5FF6,
    $5D0B00A3, $AE63AFF2, $7E8BD632, $70108C0C,
    $BBD35049, $2998DF04, $980CF42A, $9B6DF491,
    $9E7EDD53, $06918548, $58CB7E07, $3B74EF2E,
    $522FFFB1, $D24708CC, $1C7E27CD, $A4EB215B,
    $3CF1D2E2, $19B47A38, $424F7618, $35856039,
    $9D17DEE7, $27EB35E6, $C9AFF67B, $36BAF5B8,
    $09C467CD, $C18910B1, $E11DBF7B, $06CD1AF8,
    $7170C608, $2D5E3354, $D4DE495A, $64C6D006,
    $BCC0C62C, $3DD00DB3, $708F8F34, $77D51B42,
    $264F620F, $24B8D2BF, $15C1B79E, $46A52564,
    $F8D7E54E, $3E378160, $7895CDA5, $859C15A5,
    $E6459788, $C37BC75F, $DB07BA0C, $0676A3AB,
    $7F229B1E, $31842E7B, $24259FD7, $F8BEF472,
    $835FFCB8, $6DF4C1F2, $96F5B195, $FD0AF0FC,
    $B0FE134C, $E2506D3D, $4F9B12EA, $F215F225,
    $A223736F, $9FB4C428, $25D04979, $34C713F8,
    $C4618187, $EA7A6E98, $7CD16EFC, $1436876C,
    $F1544107, $BEDEEE14, $56E9AF27, $A04AA441,
    $3CF7C899, $92ECBAE6, $DD67016D, $151682EB,
    $A842EEDF, $FDBA60B4, $F1907B75, $20E3030F,
    $24D8C29E, $E139673B, $EFA63FB8, $71873054,
    $B6F2CF3B, $9F326442, $CB15A4CC, $B01A4504,
    $F1E47D8D, $844A1BE5, $BAE7DFDC, $42CBDA70,
    $CD7DAE0A, $57E85B7A, $D53F5AF6, $20CF4D8C,
    $CEA4D428, $79D130A4, $3486EBFB, $33D3CDDC,
    $77853B53, $37EFFCB5, $C5068778, $E580B3E6,
    $4E68B8F4, $C5C8B37E, $0D809EA2, $398FEB7C,
    $132A4F94, $43B7950E, $2FEE7D1C, $223613BD,
    $DD06CAA2, $37DF932B, $C4248289, $ACF3EBC3,
    $5715F6B7, $EF3478DD, $F267616F, $C148CBE4,
    $9052815E, $5E410FAB, $B48A2465, $2EDA7FA4,
    $E87B40E4, $E98EA084, $5889E9E1, $EFD390FC,
    $DD07D35B, $DB485694, $38D7E5B2, $57720101,
    $730EDEBC, $5B643113, $94917E4F, $503C2FBA,
    $646F1282, $7523D24A, $E0779695, $F9C17A8F,
    $7A5B2121, $D187B896, $29263A4D, $BA510CDF,
    $81F47C9F, $AD1163ED, $EA7B5965, $1A00726E,
    $11403092, $00DA6D77, $4A0CDD61, $AD1F4603,
    $605BDFB0, $9EEDC364, $22EBE6A8, $CEE7D28A,
    $A0E736A0, $5564A6B9, $10853209, $C7EB8F37,
    $2DE705CA, $8951570F, $DF09822B, $BD691A6C,
    $AA12E4F2, $87451C0F, $E0F6A27A, $3ADA4819,
    $4CF1764F, $0D771C2B, $67CDB156, $350D8384,
    $5938FA0F, $42399EF3, $36997B07, $0E84093D,
    $4AA93E61, $8360D87B, $1FA98B0C, $1149382C,
    $E97625A5, $0614D1B7, $0E25244B, $0C768347,
    $589E8D82, $0D2059D1, $A466BB1E, $F8DA0A82,
    $04F19130, $BA6E4EC0, $99265164, $1EE7230D,
    $50B2AD80, $EAEE6801, $8DB2A283, $EA8BF59E
   );


type
 PCast128 = ^TCast128;
 TCast128 = object(TBlockCipher64)
  protected
    KeyData: array[0..31] of DWord;
    Rounds: longword;

  public
   // code
    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;

  destructor Destroy; virtual;
 end;

//Cast256 cipher implementation
const
  S1: array[0..255] of DWord= (
    $30fb40d4, $9fa0ff0b, $6beccd2f, $3f258c7a, $1e213f2f, $9C004dd3,
    $6003e540, $cf9fc949, $bfd4af27, $88bbbdb5, $e2034090, $98d09675,
    $6e63a0e0, $15c361d2, $c2e7661d, $22d4ff8e, $28683b6f, $c07fd059,
    $ff2379c8, $775f50e2, $43c340d3, $df2f8656, $887ca41a, $a2d2bd2d,
    $a1c9e0d6, $346c4819, $61b76d87, $22540f2f, $2abe32e1, $aa54166b,
    $22568e3a, $a2d341d0, $66db40c8, $a784392f, $004dff2f, $2db9d2de,
    $97943fac, $4a97c1d8, $527644b7, $b5f437a7, $b82cbaef, $d751d159,
    $6ff7f0ed, $5a097a1f, $827b68d0, $90ecf52e, $22b0c054, $bc8e5935,
    $4b6d2f7f, $50bb64a2, $d2664910, $bee5812d, $b7332290, $e93b159f,
    $b48ee411, $4bff345d, $fd45c240, $ad31973f, $c4f6d02e, $55fc8165,
    $d5b1caad, $a1ac2dae, $a2d4b76d, $c19b0C50, $882240f2, $0c6e4f38, 
    $a4e4bfd7, $4f5ba272, $564c1d2f, $c59c5319, $b949e354, $b04669fe,
    $b1b6ab8a, $c71358dd, $6385c545, $110f935d, $57538ad5, $6a390493, 
    $e63d37e0, $2a54f6b3, $3a787d5f, $6276a0b5, $19a6fcdf, $7a42206a, 
    $29f9d4d5, $f61b1891, $bb72275e, $aa508167, $38901091, $c6b505eb, 
    $84c7cb8c, $2ad75a0f, $874a1427, $a2d1936b, $2ad286af, $aa56d291,
    $d7894360, $425c750d, $93b39e26, $187184c9, $6c00b32d, $73e2bb14,
    $a0bebc3c, $54623779, $64459eab, $3f328b82, $7718cf82, $59a2cea6,
    $04ee002e, $89fe78e6, $3fab0950, $325ff6C2, $81383f05, $6963c5c8,
    $76cb5ad6, $d49974c9, $ca180dcf, $380782d5, $c7fa5cf6, $8ac31511,
    $35e79e13, $47da91d0, $f40f9086, $a7e2419e, $31366241, $051ef495,
    $aa573b04, $4a805d8d, $548300d0, $00322a3c, $bf64cddf, $ba57a68e,
    $75c6372b, $50afd341, $a7c13275, $915a0bf5, $6b54bfab, $2b0b1426, 
    $ab4cc9d7, $449ccd82, $f7fbf265, $ab85c5f3, $1b55db94, $aad4e324,
    $cfa4bd3f, $2deaa3e2, $9e204d02, $c8bd25ac, $eadf55b3, $d5bd9e98,
    $e31231b2, $2ad5ad6c, $954329de, $adbe4528, $d8710f69, $aa51c90f,
    $aa786bf6, $22513f1e, $aa51a79b, $2ad344cc, $7b5a41f0, $d37cfbad,
    $1b069505, $41ece491, $b4c332e6, $032268d4, $c9600acc, $ce387e6d,
    $bf6bb16c, $6a70fb78, $0d03d9c9, $d4df39de, $e01063da, $4736f464,
    $5ad328d8, $b347cc96, $75bb0fc3, $98511bfb, $4ffbcc35, $b58bcf6a,
    $e11f0abc, $bfc5fe4a, $a70aec10, $ac39570a, $3f04442f, $6188b153,
    $e0397a2e, $5727cb79, $9ceb418f, $1cacd68d, $2ad37c96, $0175cb9d,
    $c69dff09, $c75b65f0, $d9db40d8, $ec0e7779, $4744ead4, $b11c3274,
    $dd24cb9e, $7e1c54bd, $f01144f9, $d2240eb1, $9675b3fd, $a3ac3755,
    $d47c27af, $51c85f4d, $56907596, $a5bb15e6, $580304f0, $ca042cf1,
    $011a37ea, $8dbfaadb, $35ba3e4a, $3526ffa0, $c37b4d09, $bc306ed9,
    $98a52666, $5648f725, $ff5e569d, $0ced63d0, $7c63b2cf, $700b45e1,
    $d5ea50f1, $85a92872, $af1fbda7, $d4234870, $a7870bf3, $2d3b4d79,
    $42e04198, $0cd0ede7, $26470db8, $f881814C, $474d6ad7, $7c0c5e5c,
    $d1231959, $381b7298, $f5d2f4db, $ab838653, $6e2f1e23, $83719c9e,
    $bd91e046, $9a56456e, $dc39200c, $20c8c571, $962bda1c, $e1e696ff,
    $b141ab08, $7cca89b9, $1a69e783, $02cc4843, $a2f7c579, $429ef47d,
    $427b169c, $5ac9f049, $dd8f0f00, $5c8165bf);
  S2: array[0..255] of DWord= (
    $1f201094, $ef0ba75b, $69e3cf7e, $393f4380, $fe61cf7a, $eec5207a,
    $55889c94, $72fc0651, $ada7ef79, $4e1d7235, $d55a63ce, $de0436ba,
    $99c430ef, $5f0c0794, $18dcdb7d, $a1d6eff3, $a0b52f7b, $59e83605,
    $ee15b094, $e9ffd909, $dc440086, $ef944459, $ba83ccb3, $e0c3cdfb,
    $d1da4181, $3b092ab1, $f997f1c1, $a5e6cf7b, $01420ddb, $e4e7ef5b,
    $25a1ff41, $e180f806, $1fc41080, $179bee7a, $d37ac6a9, $fe5830a4, 
    $98de8b7f, $77e83f4e, $79929269, $24fa9f7b, $e113c85b, $acc40083, 
    $d7503525, $f7ea615f, $62143154, $0d554b63, $5d681121, $c866c359,
    $3d63cf73, $cee234c0, $d4d87e87, $5c672b21, $071f6181, $39f7627f, 
    $361e3084, $e4eb573b, $602f64a4, $d63acd9c, $1bbc4635, $9e81032d, 
    $2701f50c, $99847ab4, $a0e3df79, $ba6cf38c, $10843094, $2537a95e, 
    $f46f6ffe, $a1ff3b1f, $208cfb6a, $8f458c74, $d9e0a227, $4ec73a34,
    $fc884f69, $3e4de8df, $ef0e0088, $3559648d, $8a45388c, $1d804366, 
    $721d9bfd, $a58684bb, $e8256333, $844e8212, $128d8098, $fed33fb4, 
    $ce280ae1, $27e19ba5, $d5a6c252, $e49754bd, $c5d655dd, $eb667064, 
    $77840b4d, $a1b6a801, $84db26a9, $e0b56714, $21f043b7, $e5d05860,
    $54f03084, $066ff472, $a31aa153, $dadc4755, $b5625dbf, $68561be6, 
    $83ca6b94, $2d6ed23b, $eccf01db, $a6d3d0ba, $b6803d5c, $af77a709, 
    $33b4a34c, $397bc8d6, $5ee22b95, $5f0e5304, $81ed6f61, $20e74364, 
    $b45e1378, $de18639b, $881ca122, $b96726d1, $8049a7e8, $22b7da7b,
    $5e552d25, $5272d237, $79d2951c, $c60d894c, $488cb402, $1ba4fe5b,
    $a4b09f6b, $1ca815cf, $a20c3005, $8871df63, $b9de2fcb, $0cc6c9e9,
    $0beeff53, $e3214517, $b4542835, $9f63293c, $ee41e729, $6e1d2d7c,
    $50045286, $1e6685f3, $f33401c6, $30a22c95, $31a70850, $60930f13,
    $73f98417, $a1269859, $ec645c44, $52c877a9, $cdff33a6, $a02b1741,
    $7cbad9a2, $2180036f, $50d99c08, $cb3f4861, $c26bd765, $64a3f6ab, 
    $80342676, $25a75e7b, $e4e6d1fc, $20c710e6, $cdf0b680, $17844d3b, 
    $31eef84d, $7e0824e4, $2ccb49eb, $846a3bae, $8ff77888, $ee5d60f6,
    $7af75673, $2fdd5cdb, $a11631c1, $30f66f43, $b3faec54, $157fd7fa, 
    $ef8579cc, $d152de58, $db2ffd5e, $8f32ce19, $306af97a, $02f03ef8, 
    $99319ad5, $c242fa0f, $a7e3ebb0, $c68e4906, $b8da230c, $80823028, 
    $dcdef3c8, $d35fb171, $088a1bc8, $bec0c560, $61a3c9e8, $bca8f54d,
    $c72feffa, $22822e99, $82c570b4, $d8d94e89, $8b1c34bc, $301e16e6,
    $273be979, $b0ffeaa6, $61d9b8c6, $00b24869, $b7ffce3f, $08dc283b, 
    $43daf65a, $f7e19798, $7619b72f, $8f1c9ba4, $dc8637a0, $16a7d3b1, 
    $9fc393b7, $a7136eeb, $c6bcc63e, $1a513742, $ef6828bc, $520365d6,
    $2d6a77ab, $3527ed4b, $821fd216, $095c6e2e, $db92f2fb, $5eea29cb, 
    $145892f5, $91584f7f, $5483697b, $2667a8cc, $85196048, $8c4bacea, 
    $833860d4, $0d23e0f9, $6c387e8a, $0ae6d249, $b284600c, $d835731d, 
    $dcb1c647, $ac4c56ea, $3ebd81b3, $230eabb0, $6438bc87, $f0b5b1fa,
    $8f5ea2b3, $fc184642, $0a036b7a, $4fb089bd, $649da589, $a345415e, 
    $5c038323, $3e5d3bb9, $43d79572, $7e6dd07c, $06dfdf1e, $6c6cc4ef,
    $7160a539, $73bfbe70, $83877605, $4523ecf1);
  S3: array[0..255] of DWord= (
    $8defc240, $25fa5d9f, $eb903dbf, $e810c907, $47607fff, $369fe44b,
    $8c1fc644, $aececa90, $beb1f9bf, $eefbcaea, $e8cf1950, $51df07ae,
    $920e8806, $f0ad0548, $e13c8d83, $927010d5, $11107d9f, $07647db9,
    $b2e3e4d4, $3d4f285e, $b9afa820, $fade82e0, $a067268b, $8272792e,
    $553fb2c0, $489ae22b, $d4ef9794, $125e3fbc, $21fffcee, $825b1bfd,
    $9255c5ed, $1257a240, $4e1a8302, $bae07fff, $528246e7, $8e57140e,
    $3373f7bf, $8c9f8188, $a6fc4ee8, $c982b5a5, $a8c01db7, $579fc264,
    $67094f31, $f2bd3f5f, $40fff7c1, $1fb78dfc, $8e6bd2c1, $437be59b,
    $99b03dbf, $b5dbc64b, $638dc0e6, $55819d99, $a197c81c, $4a012d6e, 
    $c5884a28, $ccc36f71, $b843c213, $6c0743f1, $8309893c, $0feddd5f, 
    $2f7fe850, $d7c07f7e, $02507fbf, $5afb9a04, $a747d2d0, $1651192e, 
    $af70bf3e, $58c31380, $5f98302e, $727cc3c4, $0a0fb402, $0f7fef82,
    $8c96fdad, $5d2c2aae, $8ee99a49, $50da88b8, $8427f4a0, $1eac5790, 
    $796fb449, $8252dc15, $efbd7d9b, $a672597d, $ada840d8, $45f54504, 
    $fa5d7403, $e83ec305, $4f91751a, $925669c2, $23efe941, $a903f12e, 
    $60270df2, $0276e4b6, $94fd6574, $927985b2, $8276dbcb, $02778176,
    $f8af918d, $4e48f79e, $8f616ddf, $e29d840e, $842f7d83, $340ce5c8, 
    $96bbb682, $93b4b148, $ef303cab, $984faf28, $779faf9b, $92dc560d, 
    $224d1e20, $8437aa88, $7d29dc96, $2756d3dc, $8b907cee, $b51fd240, 
    $e7c07ce3, $e566b4a1, $c3e9615e, $3cf8209d, $6094d1e3, $cd9ca341,
    $5c76460e, $00ea983b, $d4d67881, $fd47572c, $f76cedd9, $bda8229c,
    $127dadaa, $438a074e, $1f97c090, $081bdb8a, $93a07ebe, $b938ca15,
    $97b03cff, $3dc2c0f8, $8d1ab2ec, $64380e51, $68cc7bfb, $d90f2788, 
    $12490181, $5de5ffd4, $dd7ef86a, $76a2e214, $b9a40368, $925d958f,
    $4b39fffa, $ba39aee9, $a4ffd30b, $faf7933b, $6d498623, $193cbcfa, 
    $27627545, $825cf47a, $61bd8ba0, $d11e42d1, $cead04f4, $127ea392, 
    $10428db7, $8272a972, $9270c4a8, $127de50b, $285ba1c8, $3c62f44f,
    $35c0eaa5, $e805d231, $428929fb, $b4fcdf82, $4fb66a53, $0e7dc15b,
    $1f081fab, $108618ae, $fcfd086d, $f9ff2889, $694bcc11, $236a5cae, 
    $12deca4d, $2c3f8cc5, $d2d02dfe, $f8ef5896, $e4cf52da, $95155b67, 
    $494a488c, $b9b6a80c, $5c8f82bc, $89d36b45, $3a609437, $ec00c9a9, 
    $44715253, $0a874b49, $d773bc40, $7c34671c, $02717ef6, $4feb5536,
    $a2d02fff, $d2bf60c4, $d43f03c0, $50b4ef6d, $07478cd1, $006e1888, 
    $a2e53f55, $b9e6d4bc, $a2048016, $97573833, $d7207d67, $de0f8f3d, 
    $72f87b33, $abcc4f33, $7688c55d, $7b00a6b0, $947b0001, $570075d2, 
    $f9bb88f8, $8942019e, $4264a5ff, $856302e0, $72dbd92b, $ee971b69,
    $6ea22fde, $5f08ae2b, $af7a616d, $e5c98767, $cf1febd2, $61efc8c2, 
    $f1ac2571, $cc8239c2, $67214cb8, $b1e583d1, $b7dc3e62, $7f10bdce, 
    $f90a5c38, $0ff0443d, $606e6dc6, $60543a49, $5727c148, $2be98a1d, 
    $8ab41738, $20e1be24, $af96da0f, $68458425, $99833be5, $600d457d,
    $282f9350, $8334b362, $d91d1120, $2b6d8da0, $642b1e31, $9c305a00, 
    $52bce688, $1b03588a, $f7baefd5, $4142ed9c, $a4315c11, $83323ec5, 
    $dfef4636, $a133c501, $e9d3531c, $ee353783);
  S4: array[0..255] of DWord= (
    $9db30420, $1fb6e9de, $a7be7bef, $d273a298, $4a4f7bdb, $64ad8c57,
    $85510443, $fa020ed1, $7e287aff, $e60fb663, $095f35a1, $79ebf120,
    $fd059d43, $6497b7b1, $f3641f63, $241e4adf, $28147f5f, $4fa2b8cd,
    $c9430040, $0cc32220, $fdd30b30, $c0a5374f, $1d2d00d9, $24147b15,
    $ee4d111a, $0fca5167, $71ff904c, $2d195ffe, $1a05645f, $0c13fefe,
    $081b08ca, $05170121, $80530100, $e83e5efe, $ac9af4f8, $7fe72701,
    $d2b8ee5f, $06df4261, $bb9e9b8a, $7293ea25, $ce84ffdf, $f5718801,
    $3dd64b04, $a26f263b, $7ed48400, $547eebe6, $446d4ca0, $6cf3d6f5,
    $2649abdf, $aea0c7f5, $36338cc1, $503f7e93, $d3772061, $11b638e1,
    $72500e03, $f80eb2bb, $abe0502e, $ec8d77de, $57971e81, $e14f6746,
    $c9335400, $6920318f, $081dbb99, $ffc304a5, $4d351805, $7f3d5ce3,
    $a6c866c6, $5d5bcca9, $daec6fea, $9f926f91, $9f46222f, $3991467d,
    $a5bf6d8e, $1143c44f, $43958302, $d0214eeb, $022083b8, $3fb6180c,
    $18f8931e, $281658e6, $26486e3e, $8bd78a70, $7477e4c1, $b506e07c,
    $f32d0a25, $79098b02, $e4eabb81, $28123b23, $69dead38, $1574ca16,
    $df871b62, $211c40b7, $a51a9ef9, $0014377b, $041e8ac8, $09114003,
    $bd59e4d2, $e3d156d5, $4fe876d5, $2f91a340, $557be8de, $00eae4a7,
    $0ce5c2ec, $4db4bba6, $e756bdff, $dd3369ac, $ec17b035, $06572327,
    $99afc8b0, $56c8c391, $6b65811c, $5e146119, $6e85cb75, $be07c002,
    $c2325577, $893ff4ec, $5bbfc92d, $d0ec3b25, $b7801ab7, $8d6d3b24,
    $20c763ef, $c366a5fc, $9c382880, $0ace3205, $aac9548a, $eca1d7c7, 
    $041afa32, $1d16625a, $6701902c, $9b757a54, $31d477f7, $9126b031,
    $36cc6fdb, $c70b8b46, $d9e66a48, $56e55a79, $026a4ceb, $52437eff, 
    $2f8f76b4, $0df980a5, $8674cde3, $edda04eb, $17a9be04, $2c18f4df,
    $b7747f9d, $ab2af7b4, $efc34d20, $2e096b7c, $1741a254, $e5b6a035, 
    $213d42f6, $2c1c7c26, $61c2f50f, $6552daf9, $d2c231f8, $25130f69, 
    $d8167fa2, $0418f2c8, $001a96a6, $0d1526ab, $63315c21, $5e0a72ec, 
    $49bafefd, $187908d9, $8d0dbd86, $311170a7, $3e9b640c, $cc3e10d7,
    $d5cad3b6, $0caec388, $f73001e1, $6c728aff, $71eae2a1, $1f9af36e, 
    $cfcbd12f, $c1de8417, $ac07be6b, $cb44a1d8, $8b9b0f56, $013988c3, 
    $b1c52fca, $b4be31cd, $d8782806, $12a3a4e2, $6f7de532, $58fd7eb6,
    $d01ee900, $24adffc2, $f4990fc5, $9711aac5, $001d7b95, $82e5e7d2,
    $109873f6, $00613096, $c32d9521, $ada121ff, $29908415, $7fbb977f, 
    $af9eb3db, $29c9ed2a, $5ce2a465, $a730f32c, $d0aa3fe8, $8a5cc091, 
    $d49e2ce7, $0ce454a9, $d60acd86, $015f1919, $77079103, $dea03af6, 
    $78a8565e, $dee356df, $21f05cbe, $8b75e387, $b3c50651, $b8a5c3ef,
    $d8eeb6d2, $e523be77, $c2154529, $2f69efdf, $afe67afb, $f470c4b2,
    $f3e0eb5b, $d6cc9876, $39e4460c, $1fda8538, $1987832f, $ca007367, 
    $a99144f8, $296b299e, $492fc295, $9266beab, $b5676e69, $9bd3ddda,
    $df7e052f, $db25701c, $1b5e51ee, $f65324e6, $6afce36c, $0316cc04,
    $8644213e, $b7dc59d0, $7965291f, $ccd6fd43, $41823979, $932bcdf6,
    $b657c34d, $4edfd282, $7ae5290c, $3cb9536b, $851e20fe, $9833557e,
    $13ecf0b0, $d3ffb372, $3f85c5c1, $0aef7ed2);


type
  PCast256 = ^TCast256;
  TCast256= object(TBlockCipher128)
  protected
    Kr, Km: array[0..11,0..3] of DWord;
  public
    procedure InitKey(const Key; Size: longword); virtual;
    procedure Burn; virtual;
    procedure EncryptECB(const InData; var OutData); virtual;
    procedure DecryptECB(const InData; var OutData); virtual;

    destructor Destroy; virtual;
  end;


//GOST cipher implementation
const
  sTable: array[0..3, 0..255] of DWord= (
   ($00072000,$00075000,$00074800,$00071000,$00076800,$00074000,$00070000,$00077000,
    $00073000,$00075800,$00070800,$00076000,$00073800,$00077800,$00072800,$00071800,
    $0005A000,$0005D000,$0005C800,$00059000,$0005E800,$0005C000,$00058000,$0005F000,
    $0005B000,$0005D800,$00058800,$0005E000,$0005B800,$0005F800,$0005A800,$00059800,
    $00022000,$00025000,$00024800,$00021000,$00026800,$00024000,$00020000,$00027000,
    $00023000,$00025800,$00020800,$00026000,$00023800,$00027800,$00022800,$00021800,
    $00062000,$00065000,$00064800,$00061000,$00066800,$00064000,$00060000,$00067000,
    $00063000,$00065800,$00060800,$00066000,$00063800,$00067800,$00062800,$00061800,
    $00032000,$00035000,$00034800,$00031000,$00036800,$00034000,$00030000,$00037000,
    $00033000,$00035800,$00030800,$00036000,$00033800,$00037800,$00032800,$00031800,
    $0006A000,$0006D000,$0006C800,$00069000,$0006E800,$0006C000,$00068000,$0006F000,
    $0006B000,$0006D800,$00068800,$0006E000,$0006B800,$0006F800,$0006A800,$00069800,
    $0007A000,$0007D000,$0007C800,$00079000,$0007E800,$0007C000,$00078000,$0007F000,
    $0007B000,$0007D800,$00078800,$0007E000,$0007B800,$0007F800,$0007A800,$00079800,
    $00052000,$00055000,$00054800,$00051000,$00056800,$00054000,$00050000,$00057000,
    $00053000,$00055800,$00050800,$00056000,$00053800,$00057800,$00052800,$00051800,
    $00012000,$00015000,$00014800,$00011000,$00016800,$00014000,$00010000,$00017000,
    $00013000,$00015800,$00010800,$00016000,$00013800,$00017800,$00012800,$00011800,
    $0001A000,$0001D000,$0001C800,$00019000,$0001E800,$0001C000,$00018000,$0001F000,
    $0001B000,$0001D800,$00018800,$0001E000,$0001B800,$0001F800,$0001A800,$00019800,
    $00042000,$00045000,$00044800,$00041000,$00046800,$00044000,$00040000,$00047000,
    $00043000,$00045800,$00040800,$00046000,$00043800,$00047800,$00042800,$00041800,
    $0000A000,$0000D000,$0000C800,$00009000,$0000E800,$0000C000,$00008000,$0000F000,
    $0000B000,$0000D800,$00008800,$0000E000,$0000B800,$0000F800,$0000A800,$00009800,
    $00002000,$00005000,$00004800,$00001000,$00006800,$00004000,$00000000,$00007000,
    $00003000,$00005800,$00000800,$00006000,$00003800,$00007800,$00002800,$00001800,
    $0003A000,$0003D000,$0003C800,$00039000,$0003E800,$0003C000,$00038000,$0003F000,
    $0003B000,$0003D800,$00038800,$0003E000,$0003B800,$0003F800,$0003A800,$00039800,
    $0002A000,$0002D000,$0002C800,$00029000,$0002E800,$0002C000,$00028000,$0002F000,
    $0002B000,$0002D800,$00028800,$0002E000,$0002B800,$0002F800,$0002A800,$00029800,
    $0004A000,$0004D000,$0004C800,$00049000,$0004E800,$0004C000,$00048000,$0004F000,
    $0004B000,$0004D800,$00048800,$0004E000,$0004B800,$0004F800,$0004A800,$00049800),
   ($03A80000,$03C00000,$03880000,$03E80000,$03D00000,$03980000,$03A00000,$03900000,
    $03F00000,$03F80000,$03E00000,$03B80000,$03B00000,$03800000,$03C80000,$03D80000,
    $06A80000,$06C00000,$06880000,$06E80000,$06D00000,$06980000,$06A00000,$06900000,
    $06F00000,$06F80000,$06E00000,$06B80000,$06B00000,$06800000,$06C80000,$06D80000,
    $05280000,$05400000,$05080000,$05680000,$05500000,$05180000,$05200000,$05100000,
    $05700000,$05780000,$05600000,$05380000,$05300000,$05000000,$05480000,$05580000,
    $00A80000,$00C00000,$00880000,$00E80000,$00D00000,$00980000,$00A00000,$00900000,
    $00F00000,$00F80000,$00E00000,$00B80000,$00B00000,$00800000,$00C80000,$00D80000,
    $00280000,$00400000,$00080000,$00680000,$00500000,$00180000,$00200000,$00100000,
    $00700000,$00780000,$00600000,$00380000,$00300000,$00000000,$00480000,$00580000,
    $04280000,$04400000,$04080000,$04680000,$04500000,$04180000,$04200000,$04100000,
    $04700000,$04780000,$04600000,$04380000,$04300000,$04000000,$04480000,$04580000,
    $04A80000,$04C00000,$04880000,$04E80000,$04D00000,$04980000,$04A00000,$04900000,
    $04F00000,$04F80000,$04E00000,$04B80000,$04B00000,$04800000,$04C80000,$04D80000,
    $07A80000,$07C00000,$07880000,$07E80000,$07D00000,$07980000,$07A00000,$07900000,
    $07F00000,$07F80000,$07E00000,$07B80000,$07B00000,$07800000,$07C80000,$07D80000,
    $07280000,$07400000,$07080000,$07680000,$07500000,$07180000,$07200000,$07100000,
    $07700000,$07780000,$07600000,$07380000,$07300000,$07000000,$07480000,$07580000,
    $02280000,$02400000,$02080000,$02680000,$02500000,$02180000,$02200000,$02100000,
    $02700000,$02780000,$02600000,$02380000,$02300000,$02000000,$02480000,$02580000,
    $03280000,$03400000,$03080000,$03680000,$03500000,$03180000,$03200000,$03100000,
    $03700000,$03780000,$03600000,$03380000,$03300000,$03000000,$03480000,$03580000,
    $06280000,$06400000,$06080000,$06680000,$06500000,$06180000,$06200000,$06100000,
    $06700000,$06780000,$06600000,$06380000,$06300000,$06000000,$06480000,$06580000,
    $05A80000,$05C00000,$05880000,$05E80000,$05D00000,$05980000,$05A00000,$05900000,
    $05F00000,$05F80000,$05E00000,$05B80000,$05B00000,$05800000,$05C80000,$05D80000,
    $01280000,$01400000,$01080000,$01680000,$01500000,$01180000,$01200000,$01100000,
    $01700000,$01780000,$01600000,$01380000,$01300000,$01000000,$01480000,$01580000,
    $02A80000,$02C00000,$02880000,$02E80000,$02D00000,$02980000,$02A00000,$02900000,
    $02F00000,$02F80000,$02E00000,$02B80000,$02B00000,$02800000,$02C80000,$02D80000,
    $01A80000,$01C00000,$01880000,$01E80000,$01D00000,$01980000,$01A00000,$01900000,
    $01F00000,$01F80000,$01E00000,$01B80000,$01B00000,$01800000,$01C80000,$01D80000),
   ($30000002,$60000002,$38000002,$08000002,$28000002,$78000002,$68000002,$40000002,
    $20000002,$50000002,$48000002,$70000002,$00000002,$18000002,$58000002,$10000002,
    $B0000005,$E0000005,$B8000005,$88000005,$A8000005,$F8000005,$E8000005,$C0000005,
    $A0000005,$D0000005,$C8000005,$F0000005,$80000005,$98000005,$D8000005,$90000005,
    $30000005,$60000005,$38000005,$08000005,$28000005,$78000005,$68000005,$40000005,
    $20000005,$50000005,$48000005,$70000005,$00000005,$18000005,$58000005,$10000005,
    $30000000,$60000000,$38000000,$08000000,$28000000,$78000000,$68000000,$40000000,
    $20000000,$50000000,$48000000,$70000000,$00000000,$18000000,$58000000,$10000000,
    $B0000003,$E0000003,$B8000003,$88000003,$A8000003,$F8000003,$E8000003,$C0000003,
    $A0000003,$D0000003,$C8000003,$F0000003,$80000003,$98000003,$D8000003,$90000003,
    $30000001,$60000001,$38000001,$08000001,$28000001,$78000001,$68000001,$40000001,
    $20000001,$50000001,$48000001,$70000001,$00000001,$18000001,$58000001,$10000001,
    $B0000000,$E0000000,$B8000000,$88000000,$A8000000,$F8000000,$E8000000,$C0000000,
    $A0000000,$D0000000,$C8000000,$F0000000,$80000000,$98000000,$D8000000,$90000000,
    $B0000006,$E0000006,$B8000006,$88000006,$A8000006,$F8000006,$E8000006,$C0000006,
    $A0000006,$D0000006,$C8000006,$F0000006,$80000006,$98000006,$D8000006,$90000006,
    $B0000001,$E0000001,$B8000001,$88000001,$A8000001,$F8000001,$E8000001,$C0000001,
    $A0000001,$D0000001,$C8000001,$F0000001,$80000001,$98000001,$D8000001,$90000001,
    $30000003,$60000003,$38000003,$08000003,$28000003,$78000003,$68000003,$40000003,
    $20000003,$50000003,$48000003,$70000003,$00000003,$18000003,$58000003,$10000003,
    $30000004,$60000004,$38000004,$08000004,$28000004,$78000004,$68000004,$40000004,
    $20000004,$50000004,$48000004,$70000004,$00000004,$18000004,$58000004,$10000004,
    $B0000002,$E0000002,$B8000002,$88000002,$A8000002,$F8000002,$E8000002,$C0000002,
    $A0000002,$D0000002,$C8000002,$F0000002,$80000002,$98000002,$D8000002,$90000002,
    $B0000004,$E0000004,$B8000004,$88000004,$A8000004,$F8000004,$E8000004,$C0000004,
    $A0000004,$D0000004,$C8000004,$F0000004,$80000004,$98000004,$D8000004,$90000004,
    $30000006,$60000006,$38000006,$08000006,$28000006,$78000006,$68000006,$40000006,
    $20000006,$50000006,$48000006,$70000006,$00000006,$18000006,$58000006,$10000006,
    $B0000007,$E0000007,$B8000007,$88000007,$A8000007,$F8000007,$E8000007,$C0000007,
    $A0000007,$D0000007,$C8000007,$F0000007,$80000007,$98000007,$D8000007,$90000007,
    $30000007,$60000007,$38000007,$08000007,$28000007,$78000007,$68000007,$40000007,
    $20000007,$50000007,$48000007,$70000007,$00000007,$18000007,$58000007,$10000007),
   ($000000E8,$000000D8,$000000A0,$00000088,$00000098,$000000F8,$000000A8,$000000C8,
    $00000080,$000000D0,$000000F0,$000000B8,$000000B0,$000000C0,$00000090,$000000E0,
    $000007E8,$000007D8,$000007A0,$00000788,$00000798,$000007F8,$000007A8,$000007C8,
    $00000780,$000007D0,$000007F0,$000007B8,$000007B0,$000007C0,$00000790,$000007E0,
    $000006E8,$000006D8,$000006A0,$00000688,$00000698,$000006F8,$000006A8,$000006C8,
    $00000680,$000006D0,$000006F0,$000006B8,$000006B0,$000006C0,$00000690,$000006E0,
    $00000068,$00000058,$00000020,$00000008,$00000018,$00000078,$00000028,$00000048,
    $00000000,$00000050,$00000070,$00000038,$00000030,$00000040,$00000010,$00000060,
    $000002E8,$000002D8,$000002A0,$00000288,$00000298,$000002F8,$000002A8,$000002C8,
    $00000280,$000002D0,$000002F0,$000002B8,$000002B0,$000002C0,$00000290,$000002E0,
    $000003E8,$000003D8,$000003A0,$00000388,$00000398,$000003F8,$000003A8,$000003C8,
    $00000380,$000003D0,$000003F0,$000003B8,$000003B0,$000003C0,$00000390,$000003E0,
    $00000568,$00000558,$00000520,$00000508,$00000518,$00000578,$00000528,$00000548,
    $00000500,$00000550,$00000570,$00000538,$00000530,$00000540,$00000510,$00000560,
    $00000268,$00000258,$00000220,$00000208,$00000218,$00000278,$00000228,$00000248,
    $00000200,$00000250,$00000270,$00000238,$00000230,$00000240,$00000210,$00000260,
    $000004E8,$000004D8,$000004A0,$00000488,$00000498,$000004F8,$000004A8,$000004C8,
    $00000480,$000004D0,$000004F0,$000004B8,$000004B0,$000004C0,$00000490,$000004E0,
    $00000168,$00000158,$00000120,$00000108,$00000118,$00000178,$00000128,$00000148,
    $00000100,$00000150,$00000170,$00000138,$00000130,$00000140,$00000110,$00000160,
    $000001E8,$000001D8,$000001A0,$00000188,$00000198,$000001F8,$000001A8,$000001C8,
    $00000180,$000001D0,$000001F0,$000001B8,$000001B0,$000001C0,$00000190,$000001E0,
    $00000768,$00000758,$00000720,$00000708,$00000718,$00000778,$00000728,$00000748,
    $00000700,$00000750,$00000770,$00000738,$00000730,$00000740,$00000710,$00000760,
    $00000368,$00000358,$00000320,$00000308,$00000318,$00000378,$00000328,$00000348,
    $00000300,$00000350,$00000370,$00000338,$00000330,$00000340,$00000310,$00000360,
    $000005E8,$000005D8,$000005A0,$00000588,$00000598,$000005F8,$000005A8,$000005C8,
    $00000580,$000005D0,$000005F0,$000005B8,$000005B0,$000005C0,$00000590,$000005E0,
    $00000468,$00000458,$00000420,$00000408,$00000418,$00000478,$00000428,$00000448,
    $00000400,$00000450,$00000470,$00000438,$00000430,$00000440,$00000410,$00000460,
    $00000668,$00000658,$00000620,$00000608,$00000618,$00000678,$00000628,$00000648,
    $00000600,$00000650,$00000670,$00000638,$00000630,$00000640,$00000610,$00000660));

type
 PGOST = ^TGOST;
 TGOST = object(TBlockCipher64)
  protected
  KeyData: array[0..7] of DWord;
   // code

  public
   // code
    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;

  destructor Destroy; virtual;
 end;

//IDEA cipher implementation
type
 PIDEA = ^TIDEA;
 TIDEA = object(TBlockCipher64)
  protected
    EK, DK: array[0..51] of word;

  public
   // code
    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;

  destructor Destroy; virtual;
 end;

//Misty1 cipher implementation
const
  NUMROUNDSMY1 = 8;

const
  S7TABLE: array[0..$7F] of byte= (
    $1b, $32, $33, $5a, $3b, $10, $17, $54, $5b, $1a, $72, $73, $6b, $2c, $66, $49,
    $1f, $24, $13, $6c, $37, $2e, $3f, $4a, $5d, $0f, $40, $56, $25, $51, $1c, $04,
    $0b, $46, $20, $0d, $7b, $35, $44, $42, $2b, $1e, $41, $14, $4b, $79, $15, $6f,
    $0e, $55, $09, $36, $74, $0c, $67, $53, $28, $0a, $7e, $38, $02, $07, $60, $29,
    $19, $12, $65, $2f, $30, $39, $08, $68, $5f, $78, $2a, $4c, $64, $45, $75, $3d,
    $59, $48, $03, $57, $7c, $4f, $62, $3c, $1d, $21, $5e, $27, $6a, $70, $4d, $3a,
    $01, $6d, $6e, $63, $18, $77, $23, $05, $26, $76, $00, $31, $2d, $7a, $7f, $61,
    $50, $22, $11, $06, $47, $16, $52, $4e, $71, $3e, $69, $43, $34, $5c, $58, $7d);
  S9TABLE: array[0..$1FF] of Dword= (
    $1c3, $0cb, $153, $19f, $1e3, $0e9, $0fb, $035, $181, $0b9, $117, $1eb, $133, $009, $02d, $0d3,
    $0c7, $14a, $037, $07e, $0eb, $164, $193, $1d8, $0a3, $11e, $055, $02c, $01d, $1a2, $163, $118,
    $14b, $152, $1d2, $00f, $02b, $030, $13a, $0e5, $111, $138, $18e, $063, $0e3, $0c8, $1f4, $01b,
    $001, $09d, $0f8, $1a0, $16d, $1f3, $01c, $146, $07d, $0d1, $082, $1ea, $183, $12d, $0f4, $19e,
    $1d3, $0dd, $1e2, $128, $1e0, $0ec, $059, $091, $011, $12f, $026, $0dc, $0b0, $18c, $10f, $1f7,
    $0e7, $16c, $0b6, $0f9, $0d8, $151, $101, $14c, $103, $0b8, $154, $12b, $1ae, $017, $071, $00c,
    $047, $058, $07f, $1a4, $134, $129, $084, $15d, $19d, $1b2, $1a3, $048, $07c, $051, $1ca, $023,
    $13d, $1a7, $165, $03b, $042, $0da, $192, $0ce, $0c1, $06b, $09f, $1f1, $12c, $184, $0fa, $196,
    $1e1, $169, $17d, $031, $180, $10a, $094, $1da, $186, $13e, $11c, $060, $175, $1cf, $067, $119,
    $065, $068, $099, $150, $008, $007, $17c, $0b7, $024, $019, $0de, $127, $0db, $0e4, $1a9, $052,
    $109, $090, $19c, $1c1, $028, $1b3, $135, $16a, $176, $0df, $1e5, $188, $0c5, $16e, $1de, $1b1,
    $0c3, $1df, $036, $0ee, $1ee, $0f0, $093, $049, $09a, $1b6, $069, $081, $125, $00b, $05e, $0b4,
    $149, $1c7, $174, $03e, $13b, $1b7, $08e, $1c6, $0ae, $010, $095, $1ef, $04e, $0f2, $1fd, $085,
    $0fd, $0f6, $0a0, $16f, $083, $08a, $156, $09b, $13c, $107, $167, $098, $1d0, $1e9, $003, $1fe,
    $0bd, $122, $089, $0d2, $18f, $012, $033, $06a, $142, $0ed, $170, $11b, $0e2, $14f, $158, $131,
    $147, $05d, $113, $1cd, $079, $161, $1a5, $179, $09e, $1b4, $0cc, $022, $132, $01a, $0e8, $004,
    $187, $1ed, $197, $039, $1bf, $1d7, $027, $18b, $0c6, $09c, $0d0, $14e, $06c, $034, $1f2, $06e,
    $0ca, $025, $0ba, $191, $0fe, $013, $106, $02f, $1ad, $172, $1db, $0c0, $10b, $1d6, $0f5, $1ec,
    $10d, $076, $114, $1ab, $075, $10c, $1e4, $159, $054, $11f, $04b, $0c4, $1be, $0f7, $029, $0a4,
    $00e, $1f0, $077, $04d, $17a, $086, $08b, $0b3, $171, $0bf, $10e, $104, $097, $15b, $160, $168,
    $0d7, $0bb, $066, $1ce, $0fc, $092, $1c5, $06f, $016, $04a, $0a1, $139, $0af, $0f1, $190, $00a,
    $1aa, $143, $17b, $056, $18d, $166, $0d4, $1fb, $14d, $194, $19a, $087, $1f8, $123, $0a7, $1b8,
    $141, $03c, $1f9, $140, $02a, $155, $11a, $1a1, $198, $0d5, $126, $1af, $061, $12e, $157, $1dc,
    $072, $18a, $0aa, $096, $115, $0ef, $045, $07b, $08d, $145, $053, $05f, $178, $0b2, $02e, $020,
    $1d5, $03f, $1c9, $1e7, $1ac, $044, $038, $014, $0b1, $16b, $0ab, $0b5, $05a, $182, $1c8, $1d4,
    $018, $177, $064, $0cf, $06d, $100, $199, $130, $15a, $005, $120, $1bb, $1bd, $0e0, $04f, $0d6,
    $13f, $1c4, $12a, $015, $006, $0ff, $19b, $0a6, $043, $088, $050, $15f, $1e8, $121, $073, $17e,
    $0bc, $0c2, $0c9, $173, $189, $1f5, $074, $1cc, $1e6, $1a8, $195, $01f, $041, $00d, $1ba, $032,
    $03d, $1d1, $080, $0a8, $057, $1b9, $162, $148, $0d9, $105, $062, $07a, $021, $1ff, $112, $108,
    $1c0, $0a9, $11d, $1b0, $1a6, $0cd, $0f3, $05c, $102, $05b, $1d9, $144, $1f6, $0ad, $0a5, $03a,
    $1cb, $136, $17f, $046, $0e1, $01e, $1dd, $0e6, $137, $1fa, $185, $08c, $08f, $040, $1b5, $0be,
    $078, $000, $0ac, $110, $15e, $124, $002, $1bc, $0a2, $0ea, $070, $1fc, $116, $15c, $04c, $1c2);

type
 PMisty1 = ^TMisty1;
 TMisty1 = object(TBlockCipher64)
  protected

    KeyData: array[0..31] of DWord;
    function FI(const FI_IN, FI_KEY: DWord): DWord;
    function FO(const FO_IN: DWord; const k: longword): DWord;
    function FL(const FL_IN: DWord; const k: longword): DWord;
    function FLINV(const FL_IN: DWord; const k: longword): DWord;

  public
   // code
    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;

  destructor Destroy; virtual;
 end;

//RC2 cipher implementation
const
  sBoxRC2: array[0..255] of byte= (
    $D9,$78,$F9,$C4,$19,$DD,$B5,$ED,$28,$E9,$FD,$79,$4A,$A0,$D8,$9D,
    $C6,$7E,$37,$83,$2B,$76,$53,$8E,$62,$4C,$64,$88,$44,$8B,$FB,$A2,
    $17,$9A,$59,$F5,$87,$B3,$4F,$13,$61,$45,$6D,$8D,$09,$81,$7D,$32,
    $BD,$8F,$40,$EB,$86,$B7,$7B,$0B,$F0,$95,$21,$22,$5C,$6B,$4E,$82,
    $54,$D6,$65,$93,$CE,$60,$B2,$1C,$73,$56,$C0,$14,$A7,$8C,$F1,$DC,
    $12,$75,$CA,$1F,$3B,$BE,$E4,$D1,$42,$3D,$D4,$30,$A3,$3C,$B6,$26,
    $6F,$BF,$0E,$DA,$46,$69,$07,$57,$27,$F2,$1D,$9B,$BC,$94,$43,$03,
    $F8,$11,$C7,$F6,$90,$EF,$3E,$E7,$06,$C3,$D5,$2F,$C8,$66,$1E,$D7,
    $08,$E8,$EA,$DE,$80,$52,$EE,$F7,$84,$AA,$72,$AC,$35,$4D,$6A,$2A,
    $96,$1A,$D2,$71,$5A,$15,$49,$74,$4B,$9F,$D0,$5E,$04,$18,$A4,$EC,
    $C2,$E0,$41,$6E,$0F,$51,$CB,$CC,$24,$91,$AF,$50,$A1,$F4,$70,$39,
    $99,$7C,$3A,$85,$23,$B8,$B4,$7A,$FC,$02,$36,$5B,$25,$55,$97,$31,
    $2D,$5D,$FA,$98,$E3,$8A,$92,$AE,$05,$DF,$29,$10,$67,$6C,$BA,$C9,
    $D3,$00,$E6,$CF,$E1,$9E,$A8,$2C,$63,$16,$01,$3F,$58,$E2,$89,$A9,
    $0D,$38,$34,$1B,$AB,$33,$FF,$B0,$BB,$48,$0C,$5F,$B9,$B1,$CD,$2E,
    $C5,$F3,$DB,$47,$E5,$A5,$9C,$77,$0A,$A6,$20,$68,$FE,$7F,$C1,$AD);

type
 PRC2 = ^TRC2;
 TRC2 = object(TBlockCipher64)
  protected
    KeyData: array[0..63] of word;

  public
   // code
    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;

  destructor Destroy; virtual;
 end;

//RC5 cipher implementation
const
  NUMROUNDSRC5= 12;    { number of rounds must be between 12-16 }

const
   sBoxRC5: array[0..33] of dword= (
    $B7E15163,$5618CB1C,$F45044D5,$9287BE8E,$30BF3847,$CEF6B200,
    $6D2E2BB9,$0B65A572,$A99D1F2B,$47D498E4,$E60C129D,$84438C56,
    $227B060F,$C0B27FC8,$5EE9F981,$FD21733A,$9B58ECF3,$399066AC,
    $D7C7E065,$75FF5A1E,$1436D3D7,$B26E4D90,$50A5C749,$EEDD4102,
    $8D14BABB,$2B4C3474,$C983AE2D,$67BB27E6,$05F2A19F,$A42A1B58,
    $42619511,$E0990ECA,$7ED08883,$1D08023C);


type
 PRC5 = ^TRC5;
 TRC5 = object(TBlockCipher64)
  protected
    KeyData: array[0..((NUMROUNDSRC5*2)+1)] of DWord;

  public
   // code
    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;

  destructor Destroy; virtual;
 end;

//RC6 cipher implementation
const
  NUMROUNDSRC6= 20; { number of rounds must be between 16-24 }

const
  sBoxRC6: array[0..51] of DWord= (
    $B7E15163,$5618CB1C,$F45044D5,$9287BE8E,$30BF3847,$CEF6B200,
    $6D2E2BB9,$0B65A572,$A99D1F2B,$47D498E4,$E60C129D,$84438C56,
    $227B060F,$C0B27FC8,$5EE9F981,$FD21733A,$9B58ECF3,$399066AC,
    $D7C7E065,$75FF5A1E,$1436D3D7,$B26E4D90,$50A5C749,$EEDD4102,
    $8D14BABB,$2B4C3474,$C983AE2D,$67BB27E6,$05F2A19F,$A42A1B58,
    $42619511,$E0990ECA,$7ED08883,$1D08023C,$BB3F7BF5,$5976F5AE,
    $F7AE6F67,$95E5E920,$341D62D9,$D254DC92,$708C564B,$0EC3D004,
    $ACFB49BD,$4B32C376,$E96A3D2F,$87A1B6E8,$25D930A1,$C410AA5A,
    $62482413,$007F9DCC,$9EB71785,$3CEE913E);


type
  PRC6 = ^TRC6;
  TRC6 = object(TBlockCipher128)
  protected
    KeyData: array[0..((NUMROUNDSRC6*2)+3)] of DWord;
  public
    procedure InitKey(const Key; Size: longword); virtual;
    procedure Burn; virtual;
    procedure EncryptECB(const InData; var OutData); virtual;
    procedure DecryptECB(const InData; var OutData); virtual;
    destructor Destroy; virtual;
  end;


//TEA cipher implementation
const
  Delta= $9e3779b9;
  Rounds= 32;

type
 PTEA = ^TTEA;
 TTEA = object(TBlockCipher64)
  protected
    KeyData: array[0..3] of dword;

  public
   // code
    procedure Burn;virtual;
    procedure InitKey(const Key; Size: longword);virtual;
    procedure EncryptECB(const InData; var OutData);virtual;
    procedure DecryptECB(const InData; var OutData);virtual;

  destructor Destroy; virtual;
 end;

// DES and 3DES cipher implementation
const
  shifts2: array[0..15]of byte=
    (0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,0);

  des_skb: array[0..7,0..63]of dword=(
    (
    (* for C bits (numbered as per FIPS 46) 1 2 3 4 5 6 *)
    $00000000,$00000010,$20000000,$20000010,
    $00010000,$00010010,$20010000,$20010010,
    $00000800,$00000810,$20000800,$20000810,
    $00010800,$00010810,$20010800,$20010810,
    $00000020,$00000030,$20000020,$20000030,
    $00010020,$00010030,$20010020,$20010030,
    $00000820,$00000830,$20000820,$20000830,
    $00010820,$00010830,$20010820,$20010830,
    $00080000,$00080010,$20080000,$20080010,
    $00090000,$00090010,$20090000,$20090010,
    $00080800,$00080810,$20080800,$20080810,
    $00090800,$00090810,$20090800,$20090810,
    $00080020,$00080030,$20080020,$20080030,
    $00090020,$00090030,$20090020,$20090030,
    $00080820,$00080830,$20080820,$20080830,
    $00090820,$00090830,$20090820,$20090830
    ),(
    (* for C bits (numbered as per FIPS 46) 7 8 10 11 12 13 *)
    $00000000,$02000000,$00002000,$02002000,
    $00200000,$02200000,$00202000,$02202000,
    $00000004,$02000004,$00002004,$02002004,
    $00200004,$02200004,$00202004,$02202004,
    $00000400,$02000400,$00002400,$02002400,
    $00200400,$02200400,$00202400,$02202400,
    $00000404,$02000404,$00002404,$02002404,
    $00200404,$02200404,$00202404,$02202404,
    $10000000,$12000000,$10002000,$12002000,
    $10200000,$12200000,$10202000,$12202000,
    $10000004,$12000004,$10002004,$12002004,
    $10200004,$12200004,$10202004,$12202004,
    $10000400,$12000400,$10002400,$12002400,
    $10200400,$12200400,$10202400,$12202400,
    $10000404,$12000404,$10002404,$12002404,
    $10200404,$12200404,$10202404,$12202404
    ),(
    (* for C bits (numbered as per FIPS 46) 14 15 16 17 19 20 *)
    $00000000,$00000001,$00040000,$00040001,
    $01000000,$01000001,$01040000,$01040001,
    $00000002,$00000003,$00040002,$00040003,
    $01000002,$01000003,$01040002,$01040003,
    $00000200,$00000201,$00040200,$00040201,
    $01000200,$01000201,$01040200,$01040201,
    $00000202,$00000203,$00040202,$00040203,
    $01000202,$01000203,$01040202,$01040203,
    $08000000,$08000001,$08040000,$08040001,
    $09000000,$09000001,$09040000,$09040001,
    $08000002,$08000003,$08040002,$08040003,
    $09000002,$09000003,$09040002,$09040003,
    $08000200,$08000201,$08040200,$08040201,
    $09000200,$09000201,$09040200,$09040201,
    $08000202,$08000203,$08040202,$08040203,
    $09000202,$09000203,$09040202,$09040203
    ),(
    (* for C bits (numbered as per FIPS 46) 21 23 24 26 27 28 *)
    $00000000,$00100000,$00000100,$00100100,
    $00000008,$00100008,$00000108,$00100108,
    $00001000,$00101000,$00001100,$00101100,
    $00001008,$00101008,$00001108,$00101108,
    $04000000,$04100000,$04000100,$04100100,
    $04000008,$04100008,$04000108,$04100108,
    $04001000,$04101000,$04001100,$04101100,
    $04001008,$04101008,$04001108,$04101108,
    $00020000,$00120000,$00020100,$00120100,
    $00020008,$00120008,$00020108,$00120108,
    $00021000,$00121000,$00021100,$00121100,
    $00021008,$00121008,$00021108,$00121108,
    $04020000,$04120000,$04020100,$04120100,
    $04020008,$04120008,$04020108,$04120108,
    $04021000,$04121000,$04021100,$04121100,
    $04021008,$04121008,$04021108,$04121108
    ),(
    (* for D bits (numbered as per FIPS 46) 1 2 3 4 5 6 *)
    $00000000,$10000000,$00010000,$10010000,
    $00000004,$10000004,$00010004,$10010004,
    $20000000,$30000000,$20010000,$30010000,
    $20000004,$30000004,$20010004,$30010004,
    $00100000,$10100000,$00110000,$10110000,
    $00100004,$10100004,$00110004,$10110004,
    $20100000,$30100000,$20110000,$30110000,
    $20100004,$30100004,$20110004,$30110004,
    $00001000,$10001000,$00011000,$10011000,
    $00001004,$10001004,$00011004,$10011004,
    $20001000,$30001000,$20011000,$30011000,
    $20001004,$30001004,$20011004,$30011004,
    $00101000,$10101000,$00111000,$10111000,
    $00101004,$10101004,$00111004,$10111004,
    $20101000,$30101000,$20111000,$30111000,
    $20101004,$30101004,$20111004,$30111004
    ),(
    (* for D bits (numbered as per FIPS 46) 8 9 11 12 13 14 *)
    $00000000,$08000000,$00000008,$08000008,
    $00000400,$08000400,$00000408,$08000408,
    $00020000,$08020000,$00020008,$08020008,
    $00020400,$08020400,$00020408,$08020408,
    $00000001,$08000001,$00000009,$08000009,
    $00000401,$08000401,$00000409,$08000409,
    $00020001,$08020001,$00020009,$08020009,
    $00020401,$08020401,$00020409,$08020409,
    $02000000,$0A000000,$02000008,$0A000008,
    $02000400,$0A000400,$02000408,$0A000408,
    $02020000,$0A020000,$02020008,$0A020008,
    $02020400,$0A020400,$02020408,$0A020408,
    $02000001,$0A000001,$02000009,$0A000009,
    $02000401,$0A000401,$02000409,$0A000409,
    $02020001,$0A020001,$02020009,$0A020009,
    $02020401,$0A020401,$02020409,$0A020409
    ),(
    (* for D bits (numbered as per FIPS 46) 16 17 18 19 20 21 *)
    $00000000,$00000100,$00080000,$00080100,
    $01000000,$01000100,$01080000,$01080100,
    $00000010,$00000110,$00080010,$00080110,
    $01000010,$01000110,$01080010,$01080110,
    $00200000,$00200100,$00280000,$00280100,
    $01200000,$01200100,$01280000,$01280100,
    $00200010,$00200110,$00280010,$00280110,
    $01200010,$01200110,$01280010,$01280110,
    $00000200,$00000300,$00080200,$00080300,
    $01000200,$01000300,$01080200,$01080300,
    $00000210,$00000310,$00080210,$00080310,
    $01000210,$01000310,$01080210,$01080310,
    $00200200,$00200300,$00280200,$00280300,
    $01200200,$01200300,$01280200,$01280300,
    $00200210,$00200310,$00280210,$00280310,
    $01200210,$01200310,$01280210,$01280310
    ),(
    (* for D bits (numbered as per FIPS 46) 22 23 24 25 27 28 *)
    $00000000,$04000000,$00040000,$04040000,
    $00000002,$04000002,$00040002,$04040002,
    $00002000,$04002000,$00042000,$04042000,
    $00002002,$04002002,$00042002,$04042002,
    $00000020,$04000020,$00040020,$04040020,
    $00000022,$04000022,$00040022,$04040022,
    $00002020,$04002020,$00042020,$04042020,
    $00002022,$04002022,$00042022,$04042022,
    $00000800,$04000800,$00040800,$04040800,
    $00000802,$04000802,$00040802,$04040802,
    $00002800,$04002800,$00042800,$04042800,
    $00002802,$04002802,$00042802,$04042802,
    $00000820,$04000820,$00040820,$04040820,
    $00000822,$04000822,$00040822,$04040822,
    $00002820,$04002820,$00042820,$04042820,
    $00002822,$04002822,$00042822,$04042822
    ));

  des_sptrans: array[0..7,0..63] of dword=(
    (
    (* nibble 0 *)
    $02080800, $00080000, $02000002, $02080802,
    $02000000, $00080802, $00080002, $02000002,
    $00080802, $02080800, $02080000, $00000802,
    $02000802, $02000000, $00000000, $00080002,
    $00080000, $00000002, $02000800, $00080800,
    $02080802, $02080000, $00000802, $02000800,
    $00000002, $00000800, $00080800, $02080002,
    $00000800, $02000802, $02080002, $00000000,
    $00000000, $02080802, $02000800, $00080002,
    $02080800, $00080000, $00000802, $02000800,
    $02080002, $00000800, $00080800, $02000002,
    $00080802, $00000002, $02000002, $02080000,
    $02080802, $00080800, $02080000, $02000802,
    $02000000, $00000802, $00080002, $00000000,
    $00080000, $02000000, $02000802, $02080800,
    $00000002, $02080002, $00000800, $00080802
    ),(
    (* nibble 1 *)
    $40108010, $00000000, $00108000, $40100000,
    $40000010, $00008010, $40008000, $00108000,
    $00008000, $40100010, $00000010, $40008000,
    $00100010, $40108000, $40100000, $00000010,
    $00100000, $40008010, $40100010, $00008000,
    $00108010, $40000000, $00000000, $00100010,
    $40008010, $00108010, $40108000, $40000010,
    $40000000, $00100000, $00008010, $40108010,
    $00100010, $40108000, $40008000, $00108010,
    $40108010, $00100010, $40000010, $00000000,
    $40000000, $00008010, $00100000, $40100010,
    $00008000, $40000000, $00108010, $40008010,
    $40108000, $00008000, $00000000, $40000010,
    $00000010, $40108010, $00108000, $40100000,
    $40100010, $00100000, $00008010, $40008000,
    $40008010, $00000010, $40100000, $00108000
    ),(
    (* nibble 2 *)
    $04000001, $04040100, $00000100, $04000101,
    $00040001, $04000000, $04000101, $00040100,
    $04000100, $00040000, $04040000, $00000001,
    $04040101, $00000101, $00000001, $04040001,
    $00000000, $00040001, $04040100, $00000100,
    $00000101, $04040101, $00040000, $04000001,
    $04040001, $04000100, $00040101, $04040000,
    $00040100, $00000000, $04000000, $00040101,
    $04040100, $00000100, $00000001, $00040000,
    $00000101, $00040001, $04040000, $04000101,
    $00000000, $04040100, $00040100, $04040001,
    $00040001, $04000000, $04040101, $00000001,
    $00040101, $04000001, $04000000, $04040101,
    $00040000, $04000100, $04000101, $00040100,
    $04000100, $00000000, $04040001, $00000101,
    $04000001, $00040101, $00000100, $04040000
    ),(
    (* nibble 3 *)
    $00401008, $10001000, $00000008, $10401008,
    $00000000, $10400000, $10001008, $00400008,
    $10401000, $10000008, $10000000, $00001008,
    $10000008, $00401008, $00400000, $10000000,
    $10400008, $00401000, $00001000, $00000008,
    $00401000, $10001008, $10400000, $00001000,
    $00001008, $00000000, $00400008, $10401000,
    $10001000, $10400008, $10401008, $00400000,
    $10400008, $00001008, $00400000, $10000008,
    $00401000, $10001000, $00000008, $10400000,
    $10001008, $00000000, $00001000, $00400008,
    $00000000, $10400008, $10401000, $00001000,
    $10000000, $10401008, $00401008, $00400000,
    $10401008, $00000008, $10001000, $00401008,
    $00400008, $00401000, $10400000, $10001008,
    $00001008, $10000000, $10000008, $10401000
    ),(
    (* nibble 4 *)
    $08000000, $00010000, $00000400, $08010420,
    $08010020, $08000400, $00010420, $08010000,
    $00010000, $00000020, $08000020, $00010400,
    $08000420, $08010020, $08010400, $00000000,
    $00010400, $08000000, $00010020, $00000420,
    $08000400, $00010420, $00000000, $08000020,
    $00000020, $08000420, $08010420, $00010020,
    $08010000, $00000400, $00000420, $08010400,
    $08010400, $08000420, $00010020, $08010000,
    $00010000, $00000020, $08000020, $08000400,
    $08000000, $00010400, $08010420, $00000000,
    $00010420, $08000000, $00000400, $00010020,
    $08000420, $00000400, $00000000, $08010420,
    $08010020, $08010400, $00000420, $00010000,
    $00010400, $08010020, $08000400, $00000420,
    $00000020, $00010420, $08010000, $08000020
    ),(
    (* nibble 5 *)
    $80000040, $00200040, $00000000, $80202000,
    $00200040, $00002000, $80002040, $00200000,
    $00002040, $80202040, $00202000, $80000000,
    $80002000, $80000040, $80200000, $00202040,
    $00200000, $80002040, $80200040, $00000000,
    $00002000, $00000040, $80202000, $80200040,
    $80202040, $80200000, $80000000, $00002040,
    $00000040, $00202000, $00202040, $80002000,
    $00002040, $80000000, $80002000, $00202040,
    $80202000, $00200040, $00000000, $80002000,
    $80000000, $00002000, $80200040, $00200000,
    $00200040, $80202040, $00202000, $00000040,
    $80202040, $00202000, $00200000, $80002040,
    $80000040, $80200000, $00202040, $00000000,
    $00002000, $80000040, $80002040, $80202000,
    $80200000, $00002040, $00000040, $80200040
    ),(
    (* nibble 6 *)
    $00004000, $00000200, $01000200, $01000004,
    $01004204, $00004004, $00004200, $00000000,
    $01000000, $01000204, $00000204, $01004000,
    $00000004, $01004200, $01004000, $00000204,
    $01000204, $00004000, $00004004, $01004204,
    $00000000, $01000200, $01000004, $00004200,
    $01004004, $00004204, $01004200, $00000004,
    $00004204, $01004004, $00000200, $01000000,
    $00004204, $01004000, $01004004, $00000204,
    $00004000, $00000200, $01000000, $01004004,
    $01000204, $00004204, $00004200, $00000000,
    $00000200, $01000004, $00000004, $01000200,
    $00000000, $01000204, $01000200, $00004200,
    $00000204, $00004000, $01004204, $01000000,
    $01004200, $00000004, $00004004, $01004204,
    $01000004, $01004200, $01004000, $00004004
    ),(
    (* nibble 7 *)
    $20800080, $20820000, $00020080, $00000000,
    $20020000, $00800080, $20800000, $20820080,
    $00000080, $20000000, $00820000, $00020080,
    $00820080, $20020080, $20000080, $20800000,
    $00020000, $00820080, $00800080, $20020000,
    $20820080, $20000080, $00000000, $00820000,
    $20000000, $00800000, $20020080, $20800080,
    $00800000, $00020000, $20820000, $00000080,
    $00800000, $00020000, $20000080, $20820080,
    $00020080, $20000000, $00000000, $00820000,
    $20800080, $20020080, $20020000, $00800080,
    $20820000, $00000080, $00800080, $20020000,
    $20820080, $00800000, $20800000, $20000080,
    $00820000, $00020080, $20020080, $20800000,
    $00000080, $20820000, $00820080, $00000000,
    $20000000, $20800080, $00020000, $00820080
    ));

// DES and 3DES cipher implementation
type
  Pdwordarray= ^Tdwordarray;
  Tdwordarray= array[0..8191] of dword;
  
type
  TCustomDES = object(TBlockCipher64)
  protected
    procedure DoInit(KeyB: PByteArray; KeyData: PDWordArray);virtual;
    procedure EncryptBlock(const InData; var OutData; KeyData: PDWordArray);
    procedure DecryptBlock(const InData; var OutData; KeyData: PDWordArray);
  end;

type
  PDES = ^TDES;
  TDES = object (TCustomDES)
  protected
    KeyData: array[0..31] of dword;
  public
    procedure InitKey(const Key; Size: longword);virtual;
    procedure Burn; virtual;
    procedure EncryptECB(const InData; var OutData); virtual;
    procedure DecryptECB(const InData; var OutData); virtual;

    destructor Destroy; virtual;
  end;

type
  P3DES = ^T3DES;
  T3DES = object (TCustomDES)
  protected
    KeyData: array[0..2,0..31] of dword;
  public
    procedure InitKey(const Key; Size: longword);virtual;
    procedure Burn;virtual;
    procedure EncryptECB(const InData; var OutData); virtual;
    procedure DecryptECB(const InData; var OutData); virtual;

    destructor Destroy; virtual;
  end;


type
  Pwordarray= ^Twordarray;
  Twordarray= array[0..19383] of word;


// ICE, ICE2 and ThinICE  cipher implementation
type
  TCustomICE= object(TBlockCipher64)
  protected
    rounds: dword;
    ik_keysched: array[0..31,0..2] of dword;
    function f(p, sk: dword): dword;
    procedure key_sched_build(kb: pwordarray; n: dword; keyrot: pdwordarray);
    procedure InitIce(const Key; Size: longword; n: dword);
  public
    procedure Burn; virtual;
    procedure EncryptECB(const InData; var OutData); virtual;
    procedure DecryptECB(const InData; var OutData); virtual;

  end;

type
  PICE = ^TICE;
  TICE = object(TCustomICE)
  protected
  public
    procedure InitKey(const Key; Size: longword); virtual;

    destructor Destroy; virtual;
  end;

type
  PThinIce = ^TThinIce;
  TThinIce= object(TCustomICE)
  protected

  public
   procedure InitKey(const Key; Size: longword); virtual;

   destructor Destroy; virtual;
  end;

type
  PICE2 = ^TICE2;
  TICE2= object(TCustomICE)
  protected

  public
    procedure InitKey(const Key; Size: longword); virtual;

    destructor Destroy; virtual;
  end;


// Mars cipher implementation
const
  S_Box: array[0..511] of DWord= (
    $09d0c479, $28c8ffe0, $84aa6c39, $9dad7287,
    $7dff9be3, $d4268361, $c96da1d4, $7974cc93,
    $85d0582e, $2a4b5705, $1ca16a62, $c3bd279d,
    $0f1f25e5, $5160372f, $c695c1fb, $4d7ff1e4,
    $ae5f6bf4, $0d72ee46, $ff23de8a, $b1cf8e83,
    $f14902e2, $3e981e42, $8bf53eb6, $7f4bf8ac,
    $83631f83, $25970205, $76afe784, $3a7931d4,
    $4f846450, $5c64c3f6, $210a5f18, $c6986a26,
    $28f4e826, $3a60a81c, $d340a664, $7ea820c4,
    $526687c5, $7eddd12b, $32a11d1d, $9c9ef086,
    $80f6e831, $ab6f04ad, $56fb9b53, $8b2e095c,
    $b68556ae, $d2250b0d, $294a7721, $e21fb253,
    $ae136749, $e82aae86, $93365104, $99404a66,
    $78a784dc, $b69ba84b, $04046793, $23db5c1e, 
    $46cae1d6, $2fe28134, $5a223942, $1863cd5b, 
    $c190c6e3, $07dfb846, $6eb88816, $2d0dcc4a, 
    $a4ccae59, $3798670d, $cbfa9493, $4f481d45,
    $eafc8ca8, $db1129d6, $b0449e20, $0f5407fb, 
    $6167d9a8, $d1f45763, $4daa96c3, $3bec5958,
    $ababa014, $b6ccd201, $38d6279f, $02682215, 
    $8f376cd5, $092c237e, $bfc56593, $32889d2c,
    $854b3e95, $05bb9b43, $7dcd5dcd, $a02e926c,
    $fae527e5, $36a1c330, $3412e1ae, $f257f462, 
    $3c4f1d71, $30a2e809, $68e5f551, $9c61ba44,
    $5ded0ab8, $75ce09c8, $9654f93e, $698c0cca,
    $243cb3e4, $2b062b97, $0f3b8d9e, $00e050df,
    $fc5d6166, $e35f9288, $c079550d, $0591aee8, 
    $8e531e74, $75fe3578, $2f6d829a, $f60b21ae, 
    $95e8eb8d, $6699486b, $901d7d9b, $fd6d6e31,
    $1090acef, $e0670dd8, $dab2e692, $cd6d4365,
    $e5393514, $3af345f0, $6241fc4d, $460da3a3,
    $7bcf3729, $8bf1d1e0, $14aac070, $1587ed55, 
    $3afd7d3e, $d2f29e01, $29a9d1f6, $efb10c53,
    $cf3b870f, $b414935c, $664465ed, $024acac7, 
    $59a744c1, $1d2936a7, $dc580aa6, $cf574ca8, 
    $040a7a10, $6cd81807, $8a98be4c, $accea063,
    $c33e92b5, $d1e0e03d, $b322517e, $2092bd13,
    $386b2c4a, $52e8dd58, $58656dfb, $50820371,
    $41811896, $e337ef7e, $d39fb119, $c97f0df6, 
    $68fea01b, $a150a6e5, $55258962, $eb6ff41b, 
    $d7c9cd7a, $a619cd9e, $bcf09576, $2672c073,
    $f003fb3c, $4ab7a50b, $1484126a, $487ba9b1, 
    $a64fc9c6, $f6957d49, $38b06a75, $dd805fcd, 
    $63d094cf, $f51c999e, $1aa4d343, $b8495294,
    $ce9f8e99, $bffcd770, $c7c275cc, $378453a7,
    $7b21be33, $397f41bd, $4e94d131, $92cc1f98,
    $5915ea51, $99f861b7, $c9980a88, $1d74fd5f,
    $b0a495f8, $614deed0, $b5778eea, $5941792d,
    $fa90c1f8, $33f824b4, $c4965372, $3ff6d550,
    $4ca5fec0, $8630e964, $5b3fbbd6, $7da26a48,
    $b203231a, $04297514, $2d639306, $2eb13149,
    $16a45272, $532459a0, $8e5f4872, $f966c7d9,
    $07128dc0, $0d44db62, $afc8d52d, $06316131,
    $d838e7ce, $1bc41d00, $3a2e8c0f, $ea83837e,
    $b984737d, $13ba4891, $c4f8b949, $a6d6acb3,
    $a215cdce, $8359838b, $6bd1aa31, $f579dd52,
    $21b93f93, $f5176781, $187dfdde, $e94aeb76,
    $2b38fd54, $431de1da, $ab394825, $9ad3048f,
    $dfea32aa, $659473e3, $623f7863, $f3346c59,
    $ab3ab685, $3346a90b, $6b56443e, $c6de01f8,
    $8d421fc0, $9b0ed10c, $88f1a1e9, $54c1f029,
    $7dead57b, $8d7ba426, $4cf5178a, $551a7cca,
    $1a9a5f08, $fcd651b9, $25605182, $e11fc6c3,
    $b6fd9676, $337b3027, $b7c8eb14, $9e5fd030,
    $6b57e354, $ad913cf7, $7e16688d, $58872a69,
    $2c2fc7df, $e389ccc6, $30738df1, $0824a734,
    $e1797a8b, $a4a8d57b, $5b5d193b, $c8a8309b, 
    $73f9a978, $73398d32, $0f59573e, $e9df2b03, 
    $e8a5b6c8, $848d0704, $98df93c2, $720a1dc3,
    $684f259a, $943ba848, $a6370152, $863b5ea3, 
    $d17b978b, $6d9b58ef, $0a700dd4, $a73d36bf,
    $8e6a0829, $8695bc14, $e35b3447, $933ac568, 
    $8894b022, $2f511c27, $ddfbcc3c, $006662b6,
    $117c83fe, $4e12b414, $c2bca766, $3a2fec10, 
    $f4562420, $55792e2a, $46f5d857, $ceda25ce, 
    $c3601d3b, $6c00ab46, $efac9c28, $b3c35047, 
    $611dfee3, $257c3207, $fdd58482, $3b14d84f,
    $23becb64, $a075f3a3, $088f8ead, $07adf158,
    $7796943c, $facabf3d, $c09730cd, $f7679969, 
    $da44e9ed, $2c854c12, $35935fa3, $2f057d9f, 
    $690624f8, $1cb0bafd, $7b0dbdc6, $810f23bb,
    $fa929a1a, $6d969a17, $6742979b, $74ac7d05, 
    $010e65c4, $86a3d963, $f907b5a0, $d0042bd3,
    $158d7d03, $287a8255, $bba8366f, $096edc33,
    $21916a7b, $77b56b86, $951622f9, $a6c5e650,
    $8cea17d1, $cd8c62bc, $a3d63433, $358a68fd, 
    $0f9b9d3c, $d6aa295b, $fe33384a, $c000738e,
    $cd67eb2f, $e2eb6dc2, $97338b02, $06c9f246,
    $419cf1ad, $2b83c045, $3723f18a, $cb5b3089,
    $160bead7, $5d494656, $35f8a74b, $1e4e6c9e,	
    $000399bd, $67466880, $b4174831, $acf423b2,
    $ca815ab3, $5a6395e7, $302a67c5, $8bdb446b, 
    $108f8fa4, $10223eda, $92b8b48b, $7f38d0ee,
    $ab2701d4, $0262d415, $af224a30, $b3d88aba,
    $f8b2c3af, $daf7ef70, $cc97d3b7, $e9614b6c, 
    $2baebff4, $70f687cf, $386c9156, $ce092ee5,
    $01e87da6, $6ce91e6a, $bb7bcc84, $c7922c20,
    $9d3b71fd, $060e41c6, $d7590f15, $4e03bb47, 
    $183c198e, $63eeb240, $2ddbf49a, $6d5cba54, 
    $923750af, $f9e14236, $7838162b, $59726c72, 
    $81b66760, $bb2926c1, $48a0ce0d, $a6c0496d,
    $ad43507b, $718d496a, $9df057af, $44b1bde6, 
    $054356dc, $de7ced35, $d51a138b, $62088cc9, 
    $35830311, $c96efca2, $686f86ec, $8e77cb68, 
    $63e1d6b8, $c80f9778, $79c491fd, $1b4c67f2,
    $72698d7d, $5e368c31, $f7d95e2e, $a1d3493f,
    $dcd9433e, $896f1552, $4bc4ca7a, $a6d1baf4, 
    $a5a96dcc, $0bef8b46, $a169fda7, $74df40b7,
    $4e208804, $9a756607, $038e87c8, $20211e44,
    $8b7ad4bf, $c6403f35, $1848e36d, $80bdb038,
    $1e62891c, $643d2107, $bf04d6f8, $21092c8c, 
    $f644f389, $0778404e, $7b78adb8, $a2c52d53,
    $42157abe, $a2253e2e, $7bf3f4ae, $80f594f9,
    $953194e7, $77eb92ed, $b3816930, $da8d9336, 
    $bf447469, $f26d9483, $ee6faed5, $71371235, 
    $de425f73, $b4e59f43, $7dbe2d4e, $2d37b185, 
    $49dc9a63, $98c39d98, $1301c9a2, $389b1bbf,
    $0c18588d, $a421c1ba, $7aa3865c, $71e08558, 
    $3c5cfcaa, $7d239ca4, $0297d9dd, $d7dc2830, 
    $4b37802b, $7428ab54, $aeee0347, $4b3fbb85, 
    $692f2f08, $134e578e, $36d9e0bf, $ae8b5fcf,
    $edb93ecf, $2b27248e, $170eb1ef, $7dc57fd6, 
    $1e760f16, $b1136601, $864e1b9b, $d7ea7319, 
    $3ab871bd, $cfa4d76f, $e31bd782, $0dbeb469,
    $abb96061, $5370f85d, $ffb07e37, $da30d0fb,
    $ebc977b6, $0b98b40f, $3a4d0fe6, $df4fc26b,
    $159cf22a, $c298d6e2, $2b78ef6a, $61a94ac0,
    $ab561187, $14eea0f0, $df0d4164, $19af70ee);

  vk: array[0..6] of DWord= (
    $09d0c479, $28c8ffe0, $84aa6c39, $9dad7287, $7dff9be3, $d4268361,
    $c96da1d4);

type
  PMars = ^TMars;
  TMars = object(TBlockCipher128)
  protected
    KeyData: array[0..39] of DWord;
  public
    procedure InitKey(const Key; Size: longword); virtual;
    procedure Burn;  virtual;
    procedure EncryptECB(const InData; var OutData);  virtual;
    procedure DecryptECB(const InData; var OutData);  virtual;

    destructor Destroy; virtual;
  end;

// Rijndael cipher implementation
const
  MAXBC= 8;
  MAXKC= 8;

  S: array[0..255] of byte= (
     99, 124, 119, 123, 242, 107, 111, 197,  48,   1, 103,  43, 254, 215, 171, 118,
    202, 130, 201, 125, 250,  89,  71, 240, 173, 212, 162, 175, 156, 164, 114, 192,
    183, 253, 147,  38,  54,  63, 247, 204,  52, 165, 229, 241, 113, 216,  49,  21,
      4, 199,  35, 195,  24, 150,   5, 154,   7,  18, 128, 226, 235,  39, 178, 117,
      9, 131,  44,  26,  27, 110,  90, 160,  82,  59, 214, 179,  41, 227,  47, 132,
     83, 209,   0, 237,  32, 252, 177,  91, 106, 203, 190,  57,  74,  76,  88, 207, 
    208, 239, 170, 251,  67,  77,  51, 133,  69, 249,   2, 127,  80,  60, 159, 168, 
     81, 163,  64, 143, 146, 157,  56, 245, 188, 182, 218,  33,  16, 255, 243, 210, 
    205,  12,  19, 236,  95, 151,  68,  23, 196, 167, 126,  61, 100,  93,  25, 115, 
     96, 129,  79, 220,  34,  42, 144, 136,  70, 238, 184,  20, 222,  94,  11, 219, 
    224,  50,  58,  10,  73,   6,  36,  92, 194, 211, 172,  98, 145, 149, 228, 121, 
    231, 200,  55, 109, 141, 213,  78, 169, 108,  86, 244, 234, 101, 122, 174,   8, 
    186, 120,  37,  46,  28, 166, 180, 198, 232, 221, 116,  31,  75, 189, 139, 138, 
    112,  62, 181, 102,  72,   3, 246,  14,  97,  53,  87, 185, 134, 193,  29, 158,
    225, 248, 152,  17, 105, 217, 142, 148, 155,  30, 135, 233, 206,  85,  40, 223,
    140, 161, 137,  13, 191, 230,  66, 104,  65, 153,  45,  15, 176,  84, 187,  22);
  T1: array[0..255,0..3] of byte= (
    ($c6,$63,$63,$a5), ($f8,$7c,$7c,$84), ($ee,$77,$77,$99), ($f6,$7b,$7b,$8d), 
    ($ff,$f2,$f2,$0d), ($d6,$6b,$6b,$bd), ($de,$6f,$6f,$b1), ($91,$c5,$c5,$54),
    ($60,$30,$30,$50), ($02,$01,$01,$03), ($ce,$67,$67,$a9), ($56,$2b,$2b,$7d),
    ($e7,$fe,$fe,$19), ($b5,$d7,$d7,$62), ($4d,$ab,$ab,$e6), ($ec,$76,$76,$9a),
    ($8f,$ca,$ca,$45), ($1f,$82,$82,$9d), ($89,$c9,$c9,$40), ($fa,$7d,$7d,$87),
    ($ef,$fa,$fa,$15), ($b2,$59,$59,$eb), ($8e,$47,$47,$c9), ($fb,$f0,$f0,$0b), 
    ($41,$ad,$ad,$ec), ($b3,$d4,$d4,$67), ($5f,$a2,$a2,$fd), ($45,$af,$af,$ea), 
    ($23,$9c,$9c,$bf), ($53,$a4,$a4,$f7), ($e4,$72,$72,$96), ($9b,$c0,$c0,$5b), 
    ($75,$b7,$b7,$c2), ($e1,$fd,$fd,$1c), ($3d,$93,$93,$ae), ($4c,$26,$26,$6a),
    ($6c,$36,$36,$5a), ($7e,$3f,$3f,$41), ($f5,$f7,$f7,$02), ($83,$cc,$cc,$4f), 
    ($68,$34,$34,$5c), ($51,$a5,$a5,$f4), ($d1,$e5,$e5,$34), ($f9,$f1,$f1,$08), 
    ($e2,$71,$71,$93), ($ab,$d8,$d8,$73), ($62,$31,$31,$53), ($2a,$15,$15,$3f), 
    ($08,$04,$04,$0c), ($95,$c7,$c7,$52), ($46,$23,$23,$65), ($9d,$c3,$c3,$5e), 
    ($30,$18,$18,$28), ($37,$96,$96,$a1), ($0a,$05,$05,$0f), ($2f,$9a,$9a,$b5),
    ($0e,$07,$07,$09), ($24,$12,$12,$36), ($1b,$80,$80,$9b), ($df,$e2,$e2,$3d), 
    ($cd,$eb,$eb,$26), ($4e,$27,$27,$69), ($7f,$b2,$b2,$cd), ($ea,$75,$75,$9f), 
    ($12,$09,$09,$1b), ($1d,$83,$83,$9e), ($58,$2c,$2c,$74), ($34,$1a,$1a,$2e), 
    ($36,$1b,$1b,$2d), ($dc,$6e,$6e,$b2), ($b4,$5a,$5a,$ee), ($5b,$a0,$a0,$fb), 
    ($a4,$52,$52,$f6), ($76,$3b,$3b,$4d), ($b7,$d6,$d6,$61), ($7d,$b3,$b3,$ce),
    ($52,$29,$29,$7b), ($dd,$e3,$e3,$3e), ($5e,$2f,$2f,$71), ($13,$84,$84,$97), 
    ($a6,$53,$53,$f5), ($b9,$d1,$d1,$68), ($00,$00,$00,$00), ($c1,$ed,$ed,$2c), 
    ($40,$20,$20,$60), ($e3,$fc,$fc,$1f), ($79,$b1,$b1,$c8), ($b6,$5b,$5b,$ed), 
    ($d4,$6a,$6a,$be), ($8d,$cb,$cb,$46), ($67,$be,$be,$d9), ($72,$39,$39,$4b), 
    ($94,$4a,$4a,$de), ($98,$4c,$4c,$d4), ($b0,$58,$58,$e8), ($85,$cf,$cf,$4a), 
    ($bb,$d0,$d0,$6b), ($c5,$ef,$ef,$2a), ($4f,$aa,$aa,$e5), ($ed,$fb,$fb,$16),
    ($86,$43,$43,$c5), ($9a,$4d,$4d,$d7), ($66,$33,$33,$55), ($11,$85,$85,$94), 
    ($8a,$45,$45,$cf), ($e9,$f9,$f9,$10), ($04,$02,$02,$06), ($fe,$7f,$7f,$81), 
    ($a0,$50,$50,$f0), ($78,$3c,$3c,$44), ($25,$9f,$9f,$ba), ($4b,$a8,$a8,$e3), 
    ($a2,$51,$51,$f3), ($5d,$a3,$a3,$fe), ($80,$40,$40,$c0), ($05,$8f,$8f,$8a),
    ($3f,$92,$92,$ad), ($21,$9d,$9d,$bc), ($70,$38,$38,$48), ($f1,$f5,$f5,$04), 
    ($63,$bc,$bc,$df), ($77,$b6,$b6,$c1), ($af,$da,$da,$75), ($42,$21,$21,$63), 
    ($20,$10,$10,$30), ($e5,$ff,$ff,$1a), ($fd,$f3,$f3,$0e), ($bf,$d2,$d2,$6d), 
    ($81,$cd,$cd,$4c), ($18,$0c,$0c,$14), ($26,$13,$13,$35), ($c3,$ec,$ec,$2f), 
    ($be,$5f,$5f,$e1), ($35,$97,$97,$a2), ($88,$44,$44,$cc), ($2e,$17,$17,$39), 
    ($93,$c4,$c4,$57), ($55,$a7,$a7,$f2), ($fc,$7e,$7e,$82), ($7a,$3d,$3d,$47), 
    ($c8,$64,$64,$ac), ($ba,$5d,$5d,$e7), ($32,$19,$19,$2b), ($e6,$73,$73,$95), 
    ($c0,$60,$60,$a0), ($19,$81,$81,$98), ($9e,$4f,$4f,$d1), ($a3,$dc,$dc,$7f),
    ($44,$22,$22,$66), ($54,$2a,$2a,$7e), ($3b,$90,$90,$ab), ($0b,$88,$88,$83), 
    ($8c,$46,$46,$ca), ($c7,$ee,$ee,$29), ($6b,$b8,$b8,$d3), ($28,$14,$14,$3c), 
    ($a7,$de,$de,$79), ($bc,$5e,$5e,$e2), ($16,$0b,$0b,$1d), ($ad,$db,$db,$76), 
    ($db,$e0,$e0,$3b), ($64,$32,$32,$56), ($74,$3a,$3a,$4e), ($14,$0a,$0a,$1e), 
    ($92,$49,$49,$db), ($0c,$06,$06,$0a), ($48,$24,$24,$6c), ($b8,$5c,$5c,$e4),
    ($9f,$c2,$c2,$5d), ($bd,$d3,$d3,$6e), ($43,$ac,$ac,$ef), ($c4,$62,$62,$a6), 
    ($39,$91,$91,$a8), ($31,$95,$95,$a4), ($d3,$e4,$e4,$37), ($f2,$79,$79,$8b), 
    ($d5,$e7,$e7,$32), ($8b,$c8,$c8,$43), ($6e,$37,$37,$59), ($da,$6d,$6d,$b7),
    ($01,$8d,$8d,$8c), ($b1,$d5,$d5,$64), ($9c,$4e,$4e,$d2), ($49,$a9,$a9,$e0), 
    ($d8,$6c,$6c,$b4), ($ac,$56,$56,$fa), ($f3,$f4,$f4,$07), ($cf,$ea,$ea,$25), 
    ($ca,$65,$65,$af), ($f4,$7a,$7a,$8e), ($47,$ae,$ae,$e9), ($10,$08,$08,$18), 
    ($6f,$ba,$ba,$d5), ($f0,$78,$78,$88), ($4a,$25,$25,$6f), ($5c,$2e,$2e,$72),
    ($38,$1c,$1c,$24), ($57,$a6,$a6,$f1), ($73,$b4,$b4,$c7), ($97,$c6,$c6,$51), 
    ($cb,$e8,$e8,$23), ($a1,$dd,$dd,$7c), ($e8,$74,$74,$9c), ($3e,$1f,$1f,$21), 
    ($96,$4b,$4b,$dd), ($61,$bd,$bd,$dc), ($0d,$8b,$8b,$86), ($0f,$8a,$8a,$85), 
    ($e0,$70,$70,$90), ($7c,$3e,$3e,$42), ($71,$b5,$b5,$c4), ($cc,$66,$66,$aa), 
    ($90,$48,$48,$d8), ($06,$03,$03,$05), ($f7,$f6,$f6,$01), ($1c,$0e,$0e,$12), 
    ($c2,$61,$61,$a3), ($6a,$35,$35,$5f), ($ae,$57,$57,$f9), ($69,$b9,$b9,$d0), 
    ($17,$86,$86,$91), ($99,$c1,$c1,$58), ($3a,$1d,$1d,$27), ($27,$9e,$9e,$b9), 
    ($d9,$e1,$e1,$38), ($eb,$f8,$f8,$13), ($2b,$98,$98,$b3), ($22,$11,$11,$33), 
    ($d2,$69,$69,$bb), ($a9,$d9,$d9,$70), ($07,$8e,$8e,$89), ($33,$94,$94,$a7), 
    ($2d,$9b,$9b,$b6), ($3c,$1e,$1e,$22), ($15,$87,$87,$92), ($c9,$e9,$e9,$20), 
    ($87,$ce,$ce,$49), ($aa,$55,$55,$ff), ($50,$28,$28,$78), ($a5,$df,$df,$7a),
    ($03,$8c,$8c,$8f), ($59,$a1,$a1,$f8), ($09,$89,$89,$80), ($1a,$0d,$0d,$17), 
    ($65,$bf,$bf,$da), ($d7,$e6,$e6,$31), ($84,$42,$42,$c6), ($d0,$68,$68,$b8), 
    ($82,$41,$41,$c3), ($29,$99,$99,$b0), ($5a,$2d,$2d,$77), ($1e,$0f,$0f,$11), 
    ($7b,$b0,$b0,$cb), ($a8,$54,$54,$fc), ($6d,$bb,$bb,$d6), ($2c,$16,$16,$3a));
 T2: array[0..255,0..3] of byte= (
    ($a5,$c6,$63,$63), ($84,$f8,$7c,$7c), ($99,$ee,$77,$77), ($8d,$f6,$7b,$7b),
    ($0d,$ff,$f2,$f2), ($bd,$d6,$6b,$6b), ($b1,$de,$6f,$6f), ($54,$91,$c5,$c5),
    ($50,$60,$30,$30), ($03,$02,$01,$01), ($a9,$ce,$67,$67), ($7d,$56,$2b,$2b),
    ($19,$e7,$fe,$fe), ($62,$b5,$d7,$d7), ($e6,$4d,$ab,$ab), ($9a,$ec,$76,$76),
    ($45,$8f,$ca,$ca), ($9d,$1f,$82,$82), ($40,$89,$c9,$c9), ($87,$fa,$7d,$7d),
    ($15,$ef,$fa,$fa), ($eb,$b2,$59,$59), ($c9,$8e,$47,$47), ($0b,$fb,$f0,$f0),
    ($ec,$41,$ad,$ad), ($67,$b3,$d4,$d4), ($fd,$5f,$a2,$a2), ($ea,$45,$af,$af),
    ($bf,$23,$9c,$9c), ($f7,$53,$a4,$a4), ($96,$e4,$72,$72), ($5b,$9b,$c0,$c0),
    ($c2,$75,$b7,$b7), ($1c,$e1,$fd,$fd), ($ae,$3d,$93,$93), ($6a,$4c,$26,$26),
    ($5a,$6c,$36,$36), ($41,$7e,$3f,$3f), ($02,$f5,$f7,$f7), ($4f,$83,$cc,$cc),
    ($5c,$68,$34,$34), ($f4,$51,$a5,$a5), ($34,$d1,$e5,$e5), ($08,$f9,$f1,$f1),
    ($93,$e2,$71,$71), ($73,$ab,$d8,$d8), ($53,$62,$31,$31), ($3f,$2a,$15,$15), 
    ($0c,$08,$04,$04), ($52,$95,$c7,$c7), ($65,$46,$23,$23), ($5e,$9d,$c3,$c3),
    ($28,$30,$18,$18), ($a1,$37,$96,$96), ($0f,$0a,$05,$05), ($b5,$2f,$9a,$9a),
    ($09,$0e,$07,$07), ($36,$24,$12,$12), ($9b,$1b,$80,$80), ($3d,$df,$e2,$e2), 
    ($26,$cd,$eb,$eb), ($69,$4e,$27,$27), ($cd,$7f,$b2,$b2), ($9f,$ea,$75,$75),
    ($1b,$12,$09,$09), ($9e,$1d,$83,$83), ($74,$58,$2c,$2c), ($2e,$34,$1a,$1a),
    ($2d,$36,$1b,$1b), ($b2,$dc,$6e,$6e), ($ee,$b4,$5a,$5a), ($fb,$5b,$a0,$a0),
    ($f6,$a4,$52,$52), ($4d,$76,$3b,$3b), ($61,$b7,$d6,$d6), ($ce,$7d,$b3,$b3),
    ($7b,$52,$29,$29), ($3e,$dd,$e3,$e3), ($71,$5e,$2f,$2f), ($97,$13,$84,$84), 
    ($f5,$a6,$53,$53), ($68,$b9,$d1,$d1), ($00,$00,$00,$00), ($2c,$c1,$ed,$ed), 
    ($60,$40,$20,$20), ($1f,$e3,$fc,$fc), ($c8,$79,$b1,$b1), ($ed,$b6,$5b,$5b),
    ($be,$d4,$6a,$6a), ($46,$8d,$cb,$cb), ($d9,$67,$be,$be), ($4b,$72,$39,$39), 
    ($de,$94,$4a,$4a), ($d4,$98,$4c,$4c), ($e8,$b0,$58,$58), ($4a,$85,$cf,$cf),
    ($6b,$bb,$d0,$d0), ($2a,$c5,$ef,$ef), ($e5,$4f,$aa,$aa), ($16,$ed,$fb,$fb),
    ($c5,$86,$43,$43), ($d7,$9a,$4d,$4d), ($55,$66,$33,$33), ($94,$11,$85,$85), 
    ($cf,$8a,$45,$45), ($10,$e9,$f9,$f9), ($06,$04,$02,$02), ($81,$fe,$7f,$7f), 
    ($f0,$a0,$50,$50), ($44,$78,$3c,$3c), ($ba,$25,$9f,$9f), ($e3,$4b,$a8,$a8),
    ($f3,$a2,$51,$51), ($fe,$5d,$a3,$a3), ($c0,$80,$40,$40), ($8a,$05,$8f,$8f), 
    ($ad,$3f,$92,$92), ($bc,$21,$9d,$9d), ($48,$70,$38,$38), ($04,$f1,$f5,$f5), 
    ($df,$63,$bc,$bc), ($c1,$77,$b6,$b6), ($75,$af,$da,$da), ($63,$42,$21,$21),
    ($30,$20,$10,$10), ($1a,$e5,$ff,$ff), ($0e,$fd,$f3,$f3), ($6d,$bf,$d2,$d2), 
    ($4c,$81,$cd,$cd), ($14,$18,$0c,$0c), ($35,$26,$13,$13), ($2f,$c3,$ec,$ec),
    ($e1,$be,$5f,$5f), ($a2,$35,$97,$97), ($cc,$88,$44,$44), ($39,$2e,$17,$17),
    ($57,$93,$c4,$c4), ($f2,$55,$a7,$a7), ($82,$fc,$7e,$7e), ($47,$7a,$3d,$3d), 
    ($ac,$c8,$64,$64), ($e7,$ba,$5d,$5d), ($2b,$32,$19,$19), ($95,$e6,$73,$73), 
    ($a0,$c0,$60,$60), ($98,$19,$81,$81), ($d1,$9e,$4f,$4f), ($7f,$a3,$dc,$dc),
    ($66,$44,$22,$22), ($7e,$54,$2a,$2a), ($ab,$3b,$90,$90), ($83,$0b,$88,$88), 
    ($ca,$8c,$46,$46), ($29,$c7,$ee,$ee), ($d3,$6b,$b8,$b8), ($3c,$28,$14,$14),
    ($79,$a7,$de,$de), ($e2,$bc,$5e,$5e), ($1d,$16,$0b,$0b), ($76,$ad,$db,$db),
    ($3b,$db,$e0,$e0), ($56,$64,$32,$32), ($4e,$74,$3a,$3a), ($1e,$14,$0a,$0a),
    ($db,$92,$49,$49), ($0a,$0c,$06,$06), ($6c,$48,$24,$24), ($e4,$b8,$5c,$5c),
    ($5d,$9f,$c2,$c2), ($6e,$bd,$d3,$d3), ($ef,$43,$ac,$ac), ($a6,$c4,$62,$62),
    ($a8,$39,$91,$91), ($a4,$31,$95,$95), ($37,$d3,$e4,$e4), ($8b,$f2,$79,$79),
    ($32,$d5,$e7,$e7), ($43,$8b,$c8,$c8), ($59,$6e,$37,$37), ($b7,$da,$6d,$6d), 
    ($8c,$01,$8d,$8d), ($64,$b1,$d5,$d5), ($d2,$9c,$4e,$4e), ($e0,$49,$a9,$a9),
    ($b4,$d8,$6c,$6c), ($fa,$ac,$56,$56), ($07,$f3,$f4,$f4), ($25,$cf,$ea,$ea),
    ($af,$ca,$65,$65), ($8e,$f4,$7a,$7a), ($e9,$47,$ae,$ae), ($18,$10,$08,$08),
    ($d5,$6f,$ba,$ba), ($88,$f0,$78,$78), ($6f,$4a,$25,$25), ($72,$5c,$2e,$2e),
    ($24,$38,$1c,$1c), ($f1,$57,$a6,$a6), ($c7,$73,$b4,$b4), ($51,$97,$c6,$c6),
    ($23,$cb,$e8,$e8), ($7c,$a1,$dd,$dd), ($9c,$e8,$74,$74), ($21,$3e,$1f,$1f),
    ($dd,$96,$4b,$4b), ($dc,$61,$bd,$bd), ($86,$0d,$8b,$8b), ($85,$0f,$8a,$8a),
    ($90,$e0,$70,$70), ($42,$7c,$3e,$3e), ($c4,$71,$b5,$b5), ($aa,$cc,$66,$66),
    ($d8,$90,$48,$48), ($05,$06,$03,$03), ($01,$f7,$f6,$f6), ($12,$1c,$0e,$0e),
    ($a3,$c2,$61,$61), ($5f,$6a,$35,$35), ($f9,$ae,$57,$57), ($d0,$69,$b9,$b9),
    ($91,$17,$86,$86), ($58,$99,$c1,$c1), ($27,$3a,$1d,$1d), ($b9,$27,$9e,$9e),
    ($38,$d9,$e1,$e1), ($13,$eb,$f8,$f8), ($b3,$2b,$98,$98), ($33,$22,$11,$11),
    ($bb,$d2,$69,$69), ($70,$a9,$d9,$d9), ($89,$07,$8e,$8e), ($a7,$33,$94,$94),
    ($b6,$2d,$9b,$9b), ($22,$3c,$1e,$1e), ($92,$15,$87,$87), ($20,$c9,$e9,$e9),
    ($49,$87,$ce,$ce), ($ff,$aa,$55,$55), ($78,$50,$28,$28), ($7a,$a5,$df,$df),
    ($8f,$03,$8c,$8c), ($f8,$59,$a1,$a1), ($80,$09,$89,$89), ($17,$1a,$0d,$0d),
    ($da,$65,$bf,$bf), ($31,$d7,$e6,$e6), ($c6,$84,$42,$42), ($b8,$d0,$68,$68),
    ($c3,$82,$41,$41), ($b0,$29,$99,$99), ($77,$5a,$2d,$2d), ($11,$1e,$0f,$0f),
    ($cb,$7b,$b0,$b0), ($fc,$a8,$54,$54), ($d6,$6d,$bb,$bb), ($3a,$2c,$16,$16));
  T3: array[0..255,0..3] of byte= (
    ($63,$a5,$c6,$63), ($7c,$84,$f8,$7c), ($77,$99,$ee,$77), ($7b,$8d,$f6,$7b),
    ($f2,$0d,$ff,$f2), ($6b,$bd,$d6,$6b), ($6f,$b1,$de,$6f), ($c5,$54,$91,$c5),
    ($30,$50,$60,$30), ($01,$03,$02,$01), ($67,$a9,$ce,$67), ($2b,$7d,$56,$2b),
    ($fe,$19,$e7,$fe), ($d7,$62,$b5,$d7), ($ab,$e6,$4d,$ab), ($76,$9a,$ec,$76),
    ($ca,$45,$8f,$ca), ($82,$9d,$1f,$82), ($c9,$40,$89,$c9), ($7d,$87,$fa,$7d),
    ($fa,$15,$ef,$fa), ($59,$eb,$b2,$59), ($47,$c9,$8e,$47), ($f0,$0b,$fb,$f0),
    ($ad,$ec,$41,$ad), ($d4,$67,$b3,$d4), ($a2,$fd,$5f,$a2), ($af,$ea,$45,$af),
    ($9c,$bf,$23,$9c), ($a4,$f7,$53,$a4), ($72,$96,$e4,$72), ($c0,$5b,$9b,$c0),
    ($b7,$c2,$75,$b7), ($fd,$1c,$e1,$fd), ($93,$ae,$3d,$93), ($26,$6a,$4c,$26), 
    ($36,$5a,$6c,$36), ($3f,$41,$7e,$3f), ($f7,$02,$f5,$f7), ($cc,$4f,$83,$cc),
    ($34,$5c,$68,$34), ($a5,$f4,$51,$a5), ($e5,$34,$d1,$e5), ($f1,$08,$f9,$f1), 
    ($71,$93,$e2,$71), ($d8,$73,$ab,$d8), ($31,$53,$62,$31), ($15,$3f,$2a,$15),
    ($04,$0c,$08,$04), ($c7,$52,$95,$c7), ($23,$65,$46,$23), ($c3,$5e,$9d,$c3), 
    ($18,$28,$30,$18), ($96,$a1,$37,$96), ($05,$0f,$0a,$05), ($9a,$b5,$2f,$9a), 
    ($07,$09,$0e,$07), ($12,$36,$24,$12), ($80,$9b,$1b,$80), ($e2,$3d,$df,$e2), 
    ($eb,$26,$cd,$eb), ($27,$69,$4e,$27), ($b2,$cd,$7f,$b2), ($75,$9f,$ea,$75), 
    ($09,$1b,$12,$09), ($83,$9e,$1d,$83), ($2c,$74,$58,$2c), ($1a,$2e,$34,$1a), 
    ($1b,$2d,$36,$1b), ($6e,$b2,$dc,$6e), ($5a,$ee,$b4,$5a), ($a0,$fb,$5b,$a0), 
    ($52,$f6,$a4,$52), ($3b,$4d,$76,$3b), ($d6,$61,$b7,$d6), ($b3,$ce,$7d,$b3),
    ($29,$7b,$52,$29), ($e3,$3e,$dd,$e3), ($2f,$71,$5e,$2f), ($84,$97,$13,$84), 
    ($53,$f5,$a6,$53), ($d1,$68,$b9,$d1), ($00,$00,$00,$00), ($ed,$2c,$c1,$ed),
    ($20,$60,$40,$20), ($fc,$1f,$e3,$fc), ($b1,$c8,$79,$b1), ($5b,$ed,$b6,$5b), 
    ($6a,$be,$d4,$6a), ($cb,$46,$8d,$cb), ($be,$d9,$67,$be), ($39,$4b,$72,$39), 
    ($4a,$de,$94,$4a), ($4c,$d4,$98,$4c), ($58,$e8,$b0,$58), ($cf,$4a,$85,$cf),
    ($d0,$6b,$bb,$d0), ($ef,$2a,$c5,$ef), ($aa,$e5,$4f,$aa), ($fb,$16,$ed,$fb), 
    ($43,$c5,$86,$43), ($4d,$d7,$9a,$4d), ($33,$55,$66,$33), ($85,$94,$11,$85), 
    ($45,$cf,$8a,$45), ($f9,$10,$e9,$f9), ($02,$06,$04,$02), ($7f,$81,$fe,$7f), 
    ($50,$f0,$a0,$50), ($3c,$44,$78,$3c), ($9f,$ba,$25,$9f), ($a8,$e3,$4b,$a8),
    ($51,$f3,$a2,$51), ($a3,$fe,$5d,$a3), ($40,$c0,$80,$40), ($8f,$8a,$05,$8f), 
    ($92,$ad,$3f,$92), ($9d,$bc,$21,$9d), ($38,$48,$70,$38), ($f5,$04,$f1,$f5), 
    ($bc,$df,$63,$bc), ($b6,$c1,$77,$b6), ($da,$75,$af,$da), ($21,$63,$42,$21), 
    ($10,$30,$20,$10), ($ff,$1a,$e5,$ff), ($f3,$0e,$fd,$f3), ($d2,$6d,$bf,$d2), 
    ($cd,$4c,$81,$cd), ($0c,$14,$18,$0c), ($13,$35,$26,$13), ($ec,$2f,$c3,$ec), 
    ($5f,$e1,$be,$5f), ($97,$a2,$35,$97), ($44,$cc,$88,$44), ($17,$39,$2e,$17), 
    ($c4,$57,$93,$c4), ($a7,$f2,$55,$a7), ($7e,$82,$fc,$7e), ($3d,$47,$7a,$3d),
    ($64,$ac,$c8,$64), ($5d,$e7,$ba,$5d), ($19,$2b,$32,$19), ($73,$95,$e6,$73), 
    ($60,$a0,$c0,$60), ($81,$98,$19,$81), ($4f,$d1,$9e,$4f), ($dc,$7f,$a3,$dc), 
    ($22,$66,$44,$22), ($2a,$7e,$54,$2a), ($90,$ab,$3b,$90), ($88,$83,$0b,$88), 
    ($46,$ca,$8c,$46), ($ee,$29,$c7,$ee), ($b8,$d3,$6b,$b8), ($14,$3c,$28,$14), 
    ($de,$79,$a7,$de), ($5e,$e2,$bc,$5e), ($0b,$1d,$16,$0b), ($db,$76,$ad,$db), 
    ($e0,$3b,$db,$e0), ($32,$56,$64,$32), ($3a,$4e,$74,$3a), ($0a,$1e,$14,$0a), 
    ($49,$db,$92,$49), ($06,$0a,$0c,$06), ($24,$6c,$48,$24), ($5c,$e4,$b8,$5c),
    ($c2,$5d,$9f,$c2), ($d3,$6e,$bd,$d3), ($ac,$ef,$43,$ac), ($62,$a6,$c4,$62), 
    ($91,$a8,$39,$91), ($95,$a4,$31,$95), ($e4,$37,$d3,$e4), ($79,$8b,$f2,$79),
    ($e7,$32,$d5,$e7), ($c8,$43,$8b,$c8), ($37,$59,$6e,$37), ($6d,$b7,$da,$6d), 
    ($8d,$8c,$01,$8d), ($d5,$64,$b1,$d5), ($4e,$d2,$9c,$4e), ($a9,$e0,$49,$a9), 
    ($6c,$b4,$d8,$6c), ($56,$fa,$ac,$56), ($f4,$07,$f3,$f4), ($ea,$25,$cf,$ea),
    ($65,$af,$ca,$65), ($7a,$8e,$f4,$7a), ($ae,$e9,$47,$ae), ($08,$18,$10,$08), 
    ($ba,$d5,$6f,$ba), ($78,$88,$f0,$78), ($25,$6f,$4a,$25), ($2e,$72,$5c,$2e), 
    ($1c,$24,$38,$1c), ($a6,$f1,$57,$a6), ($b4,$c7,$73,$b4), ($c6,$51,$97,$c6), 
    ($e8,$23,$cb,$e8), ($dd,$7c,$a1,$dd), ($74,$9c,$e8,$74), ($1f,$21,$3e,$1f),
    ($4b,$dd,$96,$4b), ($bd,$dc,$61,$bd), ($8b,$86,$0d,$8b), ($8a,$85,$0f,$8a), 
    ($70,$90,$e0,$70), ($3e,$42,$7c,$3e), ($b5,$c4,$71,$b5), ($66,$aa,$cc,$66), 
    ($48,$d8,$90,$48), ($03,$05,$06,$03), ($f6,$01,$f7,$f6), ($0e,$12,$1c,$0e), 
    ($61,$a3,$c2,$61), ($35,$5f,$6a,$35), ($57,$f9,$ae,$57), ($b9,$d0,$69,$b9), 
    ($86,$91,$17,$86), ($c1,$58,$99,$c1), ($1d,$27,$3a,$1d), ($9e,$b9,$27,$9e), 
    ($e1,$38,$d9,$e1), ($f8,$13,$eb,$f8), ($98,$b3,$2b,$98), ($11,$33,$22,$11), 
    ($69,$bb,$d2,$69), ($d9,$70,$a9,$d9), ($8e,$89,$07,$8e), ($94,$a7,$33,$94),
    ($9b,$b6,$2d,$9b), ($1e,$22,$3c,$1e), ($87,$92,$15,$87), ($e9,$20,$c9,$e9), 
    ($ce,$49,$87,$ce), ($55,$ff,$aa,$55), ($28,$78,$50,$28), ($df,$7a,$a5,$df), 
    ($8c,$8f,$03,$8c), ($a1,$f8,$59,$a1), ($89,$80,$09,$89), ($0d,$17,$1a,$0d), 
    ($bf,$da,$65,$bf), ($e6,$31,$d7,$e6), ($42,$c6,$84,$42), ($68,$b8,$d0,$68), 
    ($41,$c3,$82,$41), ($99,$b0,$29,$99), ($2d,$77,$5a,$2d), ($0f,$11,$1e,$0f), 
    ($b0,$cb,$7b,$b0), ($54,$fc,$a8,$54), ($bb,$d6,$6d,$bb), ($16,$3a,$2c,$16));
  T4: array[0..255,0..3] of byte= (
    ($63,$63,$a5,$c6), ($7c,$7c,$84,$f8), ($77,$77,$99,$ee), ($7b,$7b,$8d,$f6),
    ($f2,$f2,$0d,$ff), ($6b,$6b,$bd,$d6), ($6f,$6f,$b1,$de), ($c5,$c5,$54,$91),
    ($30,$30,$50,$60), ($01,$01,$03,$02), ($67,$67,$a9,$ce), ($2b,$2b,$7d,$56),
    ($fe,$fe,$19,$e7), ($d7,$d7,$62,$b5), ($ab,$ab,$e6,$4d), ($76,$76,$9a,$ec), 
    ($ca,$ca,$45,$8f), ($82,$82,$9d,$1f), ($c9,$c9,$40,$89), ($7d,$7d,$87,$fa), 
    ($fa,$fa,$15,$ef), ($59,$59,$eb,$b2), ($47,$47,$c9,$8e), ($f0,$f0,$0b,$fb), 
    ($ad,$ad,$ec,$41), ($d4,$d4,$67,$b3), ($a2,$a2,$fd,$5f), ($af,$af,$ea,$45), 
    ($9c,$9c,$bf,$23), ($a4,$a4,$f7,$53), ($72,$72,$96,$e4), ($c0,$c0,$5b,$9b), 
    ($b7,$b7,$c2,$75), ($fd,$fd,$1c,$e1), ($93,$93,$ae,$3d), ($26,$26,$6a,$4c),
    ($36,$36,$5a,$6c), ($3f,$3f,$41,$7e), ($f7,$f7,$02,$f5), ($cc,$cc,$4f,$83), 
    ($34,$34,$5c,$68), ($a5,$a5,$f4,$51), ($e5,$e5,$34,$d1), ($f1,$f1,$08,$f9), 
    ($71,$71,$93,$e2), ($d8,$d8,$73,$ab), ($31,$31,$53,$62), ($15,$15,$3f,$2a), 
    ($04,$04,$0c,$08), ($c7,$c7,$52,$95), ($23,$23,$65,$46), ($c3,$c3,$5e,$9d), 
    ($18,$18,$28,$30), ($96,$96,$a1,$37), ($05,$05,$0f,$0a), ($9a,$9a,$b5,$2f),
    ($07,$07,$09,$0e), ($12,$12,$36,$24), ($80,$80,$9b,$1b), ($e2,$e2,$3d,$df), 
    ($eb,$eb,$26,$cd), ($27,$27,$69,$4e), ($b2,$b2,$cd,$7f), ($75,$75,$9f,$ea), 
    ($09,$09,$1b,$12), ($83,$83,$9e,$1d), ($2c,$2c,$74,$58), ($1a,$1a,$2e,$34), 
    ($1b,$1b,$2d,$36), ($6e,$6e,$b2,$dc), ($5a,$5a,$ee,$b4), ($a0,$a0,$fb,$5b), 
    ($52,$52,$f6,$a4), ($3b,$3b,$4d,$76), ($d6,$d6,$61,$b7), ($b3,$b3,$ce,$7d),
    ($29,$29,$7b,$52), ($e3,$e3,$3e,$dd), ($2f,$2f,$71,$5e), ($84,$84,$97,$13), 
    ($53,$53,$f5,$a6), ($d1,$d1,$68,$b9), ($00,$00,$00,$00), ($ed,$ed,$2c,$c1), 
    ($20,$20,$60,$40), ($fc,$fc,$1f,$e3), ($b1,$b1,$c8,$79), ($5b,$5b,$ed,$b6), 
    ($6a,$6a,$be,$d4), ($cb,$cb,$46,$8d), ($be,$be,$d9,$67), ($39,$39,$4b,$72), 
    ($4a,$4a,$de,$94), ($4c,$4c,$d4,$98), ($58,$58,$e8,$b0), ($cf,$cf,$4a,$85),
    ($d0,$d0,$6b,$bb), ($ef,$ef,$2a,$c5), ($aa,$aa,$e5,$4f), ($fb,$fb,$16,$ed),
    ($43,$43,$c5,$86), ($4d,$4d,$d7,$9a), ($33,$33,$55,$66), ($85,$85,$94,$11), 
    ($45,$45,$cf,$8a), ($f9,$f9,$10,$e9), ($02,$02,$06,$04), ($7f,$7f,$81,$fe), 
    ($50,$50,$f0,$a0), ($3c,$3c,$44,$78), ($9f,$9f,$ba,$25), ($a8,$a8,$e3,$4b), 
    ($51,$51,$f3,$a2), ($a3,$a3,$fe,$5d), ($40,$40,$c0,$80), ($8f,$8f,$8a,$05), 
    ($92,$92,$ad,$3f), ($9d,$9d,$bc,$21), ($38,$38,$48,$70), ($f5,$f5,$04,$f1), 
    ($bc,$bc,$df,$63), ($b6,$b6,$c1,$77), ($da,$da,$75,$af), ($21,$21,$63,$42), 
    ($10,$10,$30,$20), ($ff,$ff,$1a,$e5), ($f3,$f3,$0e,$fd), ($d2,$d2,$6d,$bf),
    ($cd,$cd,$4c,$81), ($0c,$0c,$14,$18), ($13,$13,$35,$26), ($ec,$ec,$2f,$c3), 
    ($5f,$5f,$e1,$be), ($97,$97,$a2,$35), ($44,$44,$cc,$88), ($17,$17,$39,$2e), 
    ($c4,$c4,$57,$93), ($a7,$a7,$f2,$55), ($7e,$7e,$82,$fc), ($3d,$3d,$47,$7a), 
    ($64,$64,$ac,$c8), ($5d,$5d,$e7,$ba), ($19,$19,$2b,$32), ($73,$73,$95,$e6), 
    ($60,$60,$a0,$c0), ($81,$81,$98,$19), ($4f,$4f,$d1,$9e), ($dc,$dc,$7f,$a3),
    ($22,$22,$66,$44), ($2a,$2a,$7e,$54), ($90,$90,$ab,$3b), ($88,$88,$83,$0b), 
    ($46,$46,$ca,$8c), ($ee,$ee,$29,$c7), ($b8,$b8,$d3,$6b), ($14,$14,$3c,$28), 
    ($de,$de,$79,$a7), ($5e,$5e,$e2,$bc), ($0b,$0b,$1d,$16), ($db,$db,$76,$ad), 
    ($e0,$e0,$3b,$db), ($32,$32,$56,$64), ($3a,$3a,$4e,$74), ($0a,$0a,$1e,$14), 
    ($49,$49,$db,$92), ($06,$06,$0a,$0c), ($24,$24,$6c,$48), ($5c,$5c,$e4,$b8),
    ($c2,$c2,$5d,$9f), ($d3,$d3,$6e,$bd), ($ac,$ac,$ef,$43), ($62,$62,$a6,$c4), 
    ($91,$91,$a8,$39), ($95,$95,$a4,$31), ($e4,$e4,$37,$d3), ($79,$79,$8b,$f2), 
    ($e7,$e7,$32,$d5), ($c8,$c8,$43,$8b), ($37,$37,$59,$6e), ($6d,$6d,$b7,$da), 
    ($8d,$8d,$8c,$01), ($d5,$d5,$64,$b1), ($4e,$4e,$d2,$9c), ($a9,$a9,$e0,$49), 
    ($6c,$6c,$b4,$d8), ($56,$56,$fa,$ac), ($f4,$f4,$07,$f3), ($ea,$ea,$25,$cf),
    ($65,$65,$af,$ca), ($7a,$7a,$8e,$f4), ($ae,$ae,$e9,$47), ($08,$08,$18,$10),
    ($ba,$ba,$d5,$6f), ($78,$78,$88,$f0), ($25,$25,$6f,$4a), ($2e,$2e,$72,$5c), 
    ($1c,$1c,$24,$38), ($a6,$a6,$f1,$57), ($b4,$b4,$c7,$73), ($c6,$c6,$51,$97), 
    ($e8,$e8,$23,$cb), ($dd,$dd,$7c,$a1), ($74,$74,$9c,$e8), ($1f,$1f,$21,$3e), 
    ($4b,$4b,$dd,$96), ($bd,$bd,$dc,$61), ($8b,$8b,$86,$0d), ($8a,$8a,$85,$0f), 
    ($70,$70,$90,$e0), ($3e,$3e,$42,$7c), ($b5,$b5,$c4,$71), ($66,$66,$aa,$cc), 
    ($48,$48,$d8,$90), ($03,$03,$05,$06), ($f6,$f6,$01,$f7), ($0e,$0e,$12,$1c), 
    ($61,$61,$a3,$c2), ($35,$35,$5f,$6a), ($57,$57,$f9,$ae), ($b9,$b9,$d0,$69),
    ($86,$86,$91,$17), ($c1,$c1,$58,$99), ($1d,$1d,$27,$3a), ($9e,$9e,$b9,$27), 
    ($e1,$e1,$38,$d9), ($f8,$f8,$13,$eb), ($98,$98,$b3,$2b), ($11,$11,$33,$22), 
    ($69,$69,$bb,$d2), ($d9,$d9,$70,$a9), ($8e,$8e,$89,$07), ($94,$94,$a7,$33), 
    ($9b,$9b,$b6,$2d), ($1e,$1e,$22,$3c), ($87,$87,$92,$15), ($e9,$e9,$20,$c9), 
    ($ce,$ce,$49,$87), ($55,$55,$ff,$aa), ($28,$28,$78,$50), ($df,$df,$7a,$a5),
    ($8c,$8c,$8f,$03), ($a1,$a1,$f8,$59), ($89,$89,$80,$09), ($0d,$0d,$17,$1a),
    ($bf,$bf,$da,$65), ($e6,$e6,$31,$d7), ($42,$42,$c6,$84), ($68,$68,$b8,$d0), 
    ($41,$41,$c3,$82), ($99,$99,$b0,$29), ($2d,$2d,$77,$5a), ($0f,$0f,$11,$1e), 
    ($b0,$b0,$cb,$7b), ($54,$54,$fc,$a8), ($bb,$bb,$d6,$6d), ($16,$16,$3a,$2c));
  T5: array[0..255,0..3] of byte= (
    ($51,$f4,$a7,$50), ($7e,$41,$65,$53), ($1a,$17,$a4,$c3), ($3a,$27,$5e,$96),
    ($3b,$ab,$6b,$cb), ($1f,$9d,$45,$f1), ($ac,$fa,$58,$ab), ($4b,$e3,$03,$93),
    ($20,$30,$fa,$55), ($ad,$76,$6d,$f6), ($88,$cc,$76,$91), ($f5,$02,$4c,$25),
    ($4f,$e5,$d7,$fc), ($c5,$2a,$cb,$d7), ($26,$35,$44,$80), ($b5,$62,$a3,$8f), 
    ($de,$b1,$5a,$49), ($25,$ba,$1b,$67), ($45,$ea,$0e,$98), ($5d,$fe,$c0,$e1), 
    ($c3,$2f,$75,$02), ($81,$4c,$f0,$12), ($8d,$46,$97,$a3), ($6b,$d3,$f9,$c6),
    ($03,$8f,$5f,$e7), ($15,$92,$9c,$95), ($bf,$6d,$7a,$eb), ($95,$52,$59,$da), 
    ($d4,$be,$83,$2d), ($58,$74,$21,$d3), ($49,$e0,$69,$29), ($8e,$c9,$c8,$44), 
    ($75,$c2,$89,$6a), ($f4,$8e,$79,$78), ($99,$58,$3e,$6b), ($27,$b9,$71,$dd), 
    ($be,$e1,$4f,$b6), ($f0,$88,$ad,$17), ($c9,$20,$ac,$66), ($7d,$ce,$3a,$b4), 
    ($63,$df,$4a,$18), ($e5,$1a,$31,$82), ($97,$51,$33,$60), ($62,$53,$7f,$45), 
    ($b1,$64,$77,$e0), ($bb,$6b,$ae,$84), ($fe,$81,$a0,$1c), ($f9,$08,$2b,$94), 
    ($70,$48,$68,$58), ($8f,$45,$fd,$19), ($94,$de,$6c,$87), ($52,$7b,$f8,$b7),
    ($ab,$73,$d3,$23), ($72,$4b,$02,$e2), ($e3,$1f,$8f,$57), ($66,$55,$ab,$2a), 
    ($b2,$eb,$28,$07), ($2f,$b5,$c2,$03), ($86,$c5,$7b,$9a), ($d3,$37,$08,$a5), 
    ($30,$28,$87,$f2), ($23,$bf,$a5,$b2), ($02,$03,$6a,$ba), ($ed,$16,$82,$5c),
    ($8a,$cf,$1c,$2b), ($a7,$79,$b4,$92), ($f3,$07,$f2,$f0), ($4e,$69,$e2,$a1), 
    ($65,$da,$f4,$cd), ($06,$05,$be,$d5), ($d1,$34,$62,$1f), ($c4,$a6,$fe,$8a),
    ($34,$2e,$53,$9d), ($a2,$f3,$55,$a0), ($05,$8a,$e1,$32), ($a4,$f6,$eb,$75),
    ($0b,$83,$ec,$39), ($40,$60,$ef,$aa), ($5e,$71,$9f,$06), ($bd,$6e,$10,$51), 
    ($3e,$21,$8a,$f9), ($96,$dd,$06,$3d), ($dd,$3e,$05,$ae), ($4d,$e6,$bd,$46), 
    ($91,$54,$8d,$b5), ($71,$c4,$5d,$05), ($04,$06,$d4,$6f), ($60,$50,$15,$ff), 
    ($19,$98,$fb,$24), ($d6,$bd,$e9,$97), ($89,$40,$43,$cc), ($67,$d9,$9e,$77),
    ($b0,$e8,$42,$bd), ($07,$89,$8b,$88), ($e7,$19,$5b,$38), ($79,$c8,$ee,$db),
    ($a1,$7c,$0a,$47), ($7c,$42,$0f,$e9), ($f8,$84,$1e,$c9), ($00,$00,$00,$00), 
    ($09,$80,$86,$83), ($32,$2b,$ed,$48), ($1e,$11,$70,$ac), ($6c,$5a,$72,$4e), 
    ($fd,$0e,$ff,$fb), ($0f,$85,$38,$56), ($3d,$ae,$d5,$1e), ($36,$2d,$39,$27), 
    ($0a,$0f,$d9,$64), ($68,$5c,$a6,$21), ($9b,$5b,$54,$d1), ($24,$36,$2e,$3a), 
    ($0c,$0a,$67,$b1), ($93,$57,$e7,$0f), ($b4,$ee,$96,$d2), ($1b,$9b,$91,$9e),
    ($80,$c0,$c5,$4f), ($61,$dc,$20,$a2), ($5a,$77,$4b,$69), ($1c,$12,$1a,$16), 
    ($e2,$93,$ba,$0a), ($c0,$a0,$2a,$e5), ($3c,$22,$e0,$43), ($12,$1b,$17,$1d), 
    ($0e,$09,$0d,$0b), ($f2,$8b,$c7,$ad), ($2d,$b6,$a8,$b9), ($14,$1e,$a9,$c8), 
    ($57,$f1,$19,$85), ($af,$75,$07,$4c), ($ee,$99,$dd,$bb), ($a3,$7f,$60,$fd), 
    ($f7,$01,$26,$9f), ($5c,$72,$f5,$bc), ($44,$66,$3b,$c5), ($5b,$fb,$7e,$34), 
    ($8b,$43,$29,$76), ($cb,$23,$c6,$dc), ($b6,$ed,$fc,$68), ($b8,$e4,$f1,$63), 
    ($d7,$31,$dc,$ca), ($42,$63,$85,$10), ($13,$97,$22,$40), ($84,$c6,$11,$20),
    ($85,$4a,$24,$7d), ($d2,$bb,$3d,$f8), ($ae,$f9,$32,$11), ($c7,$29,$a1,$6d), 
    ($1d,$9e,$2f,$4b), ($dc,$b2,$30,$f3), ($0d,$86,$52,$ec), ($77,$c1,$e3,$d0), 
    ($2b,$b3,$16,$6c), ($a9,$70,$b9,$99), ($11,$94,$48,$fa), ($47,$e9,$64,$22),
    ($a8,$fc,$8c,$c4), ($a0,$f0,$3f,$1a), ($56,$7d,$2c,$d8), ($22,$33,$90,$ef), 
    ($87,$49,$4e,$c7), ($d9,$38,$d1,$c1), ($8c,$ca,$a2,$fe), ($98,$d4,$0b,$36),
    ($a6,$f5,$81,$cf), ($a5,$7a,$de,$28), ($da,$b7,$8e,$26), ($3f,$ad,$bf,$a4),
    ($2c,$3a,$9d,$e4), ($50,$78,$92,$0d), ($6a,$5f,$cc,$9b), ($54,$7e,$46,$62), 
    ($f6,$8d,$13,$c2), ($90,$d8,$b8,$e8), ($2e,$39,$f7,$5e), ($82,$c3,$af,$f5), 
    ($9f,$5d,$80,$be), ($69,$d0,$93,$7c), ($6f,$d5,$2d,$a9), ($cf,$25,$12,$b3), 
    ($c8,$ac,$99,$3b), ($10,$18,$7d,$a7), ($e8,$9c,$63,$6e), ($db,$3b,$bb,$7b),
    ($cd,$26,$78,$09), ($6e,$59,$18,$f4), ($ec,$9a,$b7,$01), ($83,$4f,$9a,$a8),
    ($e6,$95,$6e,$65), ($aa,$ff,$e6,$7e), ($21,$bc,$cf,$08), ($ef,$15,$e8,$e6), 
    ($ba,$e7,$9b,$d9), ($4a,$6f,$36,$ce), ($ea,$9f,$09,$d4), ($29,$b0,$7c,$d6), 
    ($31,$a4,$b2,$af), ($2a,$3f,$23,$31), ($c6,$a5,$94,$30), ($35,$a2,$66,$c0), 
    ($74,$4e,$bc,$37), ($fc,$82,$ca,$a6), ($e0,$90,$d0,$b0), ($33,$a7,$d8,$15), 
    ($f1,$04,$98,$4a), ($41,$ec,$da,$f7), ($7f,$cd,$50,$0e), ($17,$91,$f6,$2f),
    ($76,$4d,$d6,$8d), ($43,$ef,$b0,$4d), ($cc,$aa,$4d,$54), ($e4,$96,$04,$df), 
    ($9e,$d1,$b5,$e3), ($4c,$6a,$88,$1b), ($c1,$2c,$1f,$b8), ($46,$65,$51,$7f), 
    ($9d,$5e,$ea,$04), ($01,$8c,$35,$5d), ($fa,$87,$74,$73), ($fb,$0b,$41,$2e), 
    ($b3,$67,$1d,$5a), ($92,$db,$d2,$52), ($e9,$10,$56,$33), ($6d,$d6,$47,$13), 
    ($9a,$d7,$61,$8c), ($37,$a1,$0c,$7a), ($59,$f8,$14,$8e), ($eb,$13,$3c,$89), 
    ($ce,$a9,$27,$ee), ($b7,$61,$c9,$35), ($e1,$1c,$e5,$ed), ($7a,$47,$b1,$3c), 
    ($9c,$d2,$df,$59), ($55,$f2,$73,$3f), ($18,$14,$ce,$79), ($73,$c7,$37,$bf),
    ($53,$f7,$cd,$ea), ($5f,$fd,$aa,$5b), ($df,$3d,$6f,$14), ($78,$44,$db,$86), 
    ($ca,$af,$f3,$81), ($b9,$68,$c4,$3e), ($38,$24,$34,$2c), ($c2,$a3,$40,$5f),
    ($16,$1d,$c3,$72), ($bc,$e2,$25,$0c), ($28,$3c,$49,$8b), ($ff,$0d,$95,$41),
    ($39,$a8,$01,$71), ($08,$0c,$b3,$de), ($d8,$b4,$e4,$9c), ($64,$56,$c1,$90),
    ($7b,$cb,$84,$61), ($d5,$32,$b6,$70), ($48,$6c,$5c,$74), ($d0,$b8,$57,$42));
  T6: array[0..255,0..3] of byte= (
    ($50,$51,$f4,$a7), ($53,$7e,$41,$65), ($c3,$1a,$17,$a4), ($96,$3a,$27,$5e),
    ($cb,$3b,$ab,$6b), ($f1,$1f,$9d,$45), ($ab,$ac,$fa,$58), ($93,$4b,$e3,$03),
    ($55,$20,$30,$fa), ($f6,$ad,$76,$6d), ($91,$88,$cc,$76), ($25,$f5,$02,$4c),
    ($fc,$4f,$e5,$d7), ($d7,$c5,$2a,$cb), ($80,$26,$35,$44), ($8f,$b5,$62,$a3), 
    ($49,$de,$b1,$5a), ($67,$25,$ba,$1b), ($98,$45,$ea,$0e), ($e1,$5d,$fe,$c0), 
    ($02,$c3,$2f,$75), ($12,$81,$4c,$f0), ($a3,$8d,$46,$97), ($c6,$6b,$d3,$f9), 
    ($e7,$03,$8f,$5f), ($95,$15,$92,$9c), ($eb,$bf,$6d,$7a), ($da,$95,$52,$59), 
    ($2d,$d4,$be,$83), ($d3,$58,$74,$21), ($29,$49,$e0,$69), ($44,$8e,$c9,$c8), 
    ($6a,$75,$c2,$89), ($78,$f4,$8e,$79), ($6b,$99,$58,$3e), ($dd,$27,$b9,$71), 
    ($b6,$be,$e1,$4f), ($17,$f0,$88,$ad), ($66,$c9,$20,$ac), ($b4,$7d,$ce,$3a),
    ($18,$63,$df,$4a), ($82,$e5,$1a,$31), ($60,$97,$51,$33), ($45,$62,$53,$7f), 
    ($e0,$b1,$64,$77), ($84,$bb,$6b,$ae), ($1c,$fe,$81,$a0), ($94,$f9,$08,$2b), 
    ($58,$70,$48,$68), ($19,$8f,$45,$fd), ($87,$94,$de,$6c), ($b7,$52,$7b,$f8), 
    ($23,$ab,$73,$d3), ($e2,$72,$4b,$02), ($57,$e3,$1f,$8f), ($2a,$66,$55,$ab), 
    ($07,$b2,$eb,$28), ($03,$2f,$b5,$c2), ($9a,$86,$c5,$7b), ($a5,$d3,$37,$08), 
    ($f2,$30,$28,$87), ($b2,$23,$bf,$a5), ($ba,$02,$03,$6a), ($5c,$ed,$16,$82), 
    ($2b,$8a,$cf,$1c), ($92,$a7,$79,$b4), ($f0,$f3,$07,$f2), ($a1,$4e,$69,$e2),
    ($cd,$65,$da,$f4), ($d5,$06,$05,$be), ($1f,$d1,$34,$62), ($8a,$c4,$a6,$fe),
    ($9d,$34,$2e,$53), ($a0,$a2,$f3,$55), ($32,$05,$8a,$e1), ($75,$a4,$f6,$eb),
    ($39,$0b,$83,$ec), ($aa,$40,$60,$ef), ($06,$5e,$71,$9f), ($51,$bd,$6e,$10), 
    ($f9,$3e,$21,$8a), ($3d,$96,$dd,$06), ($ae,$dd,$3e,$05), ($46,$4d,$e6,$bd), 
    ($b5,$91,$54,$8d), ($05,$71,$c4,$5d), ($6f,$04,$06,$d4), ($ff,$60,$50,$15), 
    ($24,$19,$98,$fb), ($97,$d6,$bd,$e9), ($cc,$89,$40,$43), ($77,$67,$d9,$9e),
    ($bd,$b0,$e8,$42), ($88,$07,$89,$8b), ($38,$e7,$19,$5b), ($db,$79,$c8,$ee),
    ($47,$a1,$7c,$0a), ($e9,$7c,$42,$0f), ($c9,$f8,$84,$1e), ($00,$00,$00,$00), 
    ($83,$09,$80,$86), ($48,$32,$2b,$ed), ($ac,$1e,$11,$70), ($4e,$6c,$5a,$72), 
    ($fb,$fd,$0e,$ff), ($56,$0f,$85,$38), ($1e,$3d,$ae,$d5), ($27,$36,$2d,$39), 
    ($64,$0a,$0f,$d9), ($21,$68,$5c,$a6), ($d1,$9b,$5b,$54), ($3a,$24,$36,$2e), 
    ($b1,$0c,$0a,$67), ($0f,$93,$57,$e7), ($d2,$b4,$ee,$96), ($9e,$1b,$9b,$91), 
    ($4f,$80,$c0,$c5), ($a2,$61,$dc,$20), ($69,$5a,$77,$4b), ($16,$1c,$12,$1a), 
    ($0a,$e2,$93,$ba), ($e5,$c0,$a0,$2a), ($43,$3c,$22,$e0), ($1d,$12,$1b,$17), 
    ($0b,$0e,$09,$0d), ($ad,$f2,$8b,$c7), ($b9,$2d,$b6,$a8), ($c8,$14,$1e,$a9), 
    ($85,$57,$f1,$19), ($4c,$af,$75,$07), ($bb,$ee,$99,$dd), ($fd,$a3,$7f,$60),
    ($9f,$f7,$01,$26), ($bc,$5c,$72,$f5), ($c5,$44,$66,$3b), ($34,$5b,$fb,$7e), 
    ($76,$8b,$43,$29), ($dc,$cb,$23,$c6), ($68,$b6,$ed,$fc), ($63,$b8,$e4,$f1), 
    ($ca,$d7,$31,$dc), ($10,$42,$63,$85), ($40,$13,$97,$22), ($20,$84,$c6,$11), 
    ($7d,$85,$4a,$24), ($f8,$d2,$bb,$3d), ($11,$ae,$f9,$32), ($6d,$c7,$29,$a1), 
    ($4b,$1d,$9e,$2f), ($f3,$dc,$b2,$30), ($ec,$0d,$86,$52), ($d0,$77,$c1,$e3), 
    ($6c,$2b,$b3,$16), ($99,$a9,$70,$b9), ($fa,$11,$94,$48), ($22,$47,$e9,$64), 
    ($c4,$a8,$fc,$8c), ($1a,$a0,$f0,$3f), ($d8,$56,$7d,$2c), ($ef,$22,$33,$90),
    ($c7,$87,$49,$4e), ($c1,$d9,$38,$d1), ($fe,$8c,$ca,$a2), ($36,$98,$d4,$0b),
    ($cf,$a6,$f5,$81), ($28,$a5,$7a,$de), ($26,$da,$b7,$8e), ($a4,$3f,$ad,$bf),
    ($e4,$2c,$3a,$9d), ($0d,$50,$78,$92), ($9b,$6a,$5f,$cc), ($62,$54,$7e,$46), 
    ($c2,$f6,$8d,$13), ($e8,$90,$d8,$b8), ($5e,$2e,$39,$f7), ($f5,$82,$c3,$af), 
    ($be,$9f,$5d,$80), ($7c,$69,$d0,$93), ($a9,$6f,$d5,$2d), ($b3,$cf,$25,$12), 
    ($3b,$c8,$ac,$99), ($a7,$10,$18,$7d), ($6e,$e8,$9c,$63), ($7b,$db,$3b,$bb),
    ($09,$cd,$26,$78), ($f4,$6e,$59,$18), ($01,$ec,$9a,$b7), ($a8,$83,$4f,$9a),
    ($65,$e6,$95,$6e), ($7e,$aa,$ff,$e6), ($08,$21,$bc,$cf), ($e6,$ef,$15,$e8),
    ($d9,$ba,$e7,$9b), ($ce,$4a,$6f,$36), ($d4,$ea,$9f,$09), ($d6,$29,$b0,$7c),
    ($af,$31,$a4,$b2), ($31,$2a,$3f,$23), ($30,$c6,$a5,$94), ($c0,$35,$a2,$66),
    ($37,$74,$4e,$bc), ($a6,$fc,$82,$ca), ($b0,$e0,$90,$d0), ($15,$33,$a7,$d8),
    ($4a,$f1,$04,$98), ($f7,$41,$ec,$da), ($0e,$7f,$cd,$50), ($2f,$17,$91,$f6),
    ($8d,$76,$4d,$d6), ($4d,$43,$ef,$b0), ($54,$cc,$aa,$4d), ($df,$e4,$96,$04),
    ($e3,$9e,$d1,$b5), ($1b,$4c,$6a,$88), ($b8,$c1,$2c,$1f), ($7f,$46,$65,$51),
    ($04,$9d,$5e,$ea), ($5d,$01,$8c,$35), ($73,$fa,$87,$74), ($2e,$fb,$0b,$41),
    ($5a,$b3,$67,$1d), ($52,$92,$db,$d2), ($33,$e9,$10,$56), ($13,$6d,$d6,$47),
    ($8c,$9a,$d7,$61), ($7a,$37,$a1,$0c), ($8e,$59,$f8,$14), ($89,$eb,$13,$3c),
    ($ee,$ce,$a9,$27), ($35,$b7,$61,$c9), ($ed,$e1,$1c,$e5), ($3c,$7a,$47,$b1),
    ($59,$9c,$d2,$df), ($3f,$55,$f2,$73), ($79,$18,$14,$ce), ($bf,$73,$c7,$37),
    ($ea,$53,$f7,$cd), ($5b,$5f,$fd,$aa), ($14,$df,$3d,$6f), ($86,$78,$44,$db),
    ($81,$ca,$af,$f3), ($3e,$b9,$68,$c4), ($2c,$38,$24,$34), ($5f,$c2,$a3,$40),
    ($72,$16,$1d,$c3), ($0c,$bc,$e2,$25), ($8b,$28,$3c,$49), ($41,$ff,$0d,$95),
    ($71,$39,$a8,$01), ($de,$08,$0c,$b3), ($9c,$d8,$b4,$e4), ($90,$64,$56,$c1),
    ($61,$7b,$cb,$84), ($70,$d5,$32,$b6), ($74,$48,$6c,$5c), ($42,$d0,$b8,$57));
  T7: array[0..255,0..3] of byte= (
    ($a7,$50,$51,$f4), ($65,$53,$7e,$41), ($a4,$c3,$1a,$17), ($5e,$96,$3a,$27),
    ($6b,$cb,$3b,$ab), ($45,$f1,$1f,$9d), ($58,$ab,$ac,$fa), ($03,$93,$4b,$e3),
    ($fa,$55,$20,$30), ($6d,$f6,$ad,$76), ($76,$91,$88,$cc), ($4c,$25,$f5,$02),
    ($d7,$fc,$4f,$e5), ($cb,$d7,$c5,$2a), ($44,$80,$26,$35), ($a3,$8f,$b5,$62),
    ($5a,$49,$de,$b1), ($1b,$67,$25,$ba), ($0e,$98,$45,$ea), ($c0,$e1,$5d,$fe),
    ($75,$02,$c3,$2f), ($f0,$12,$81,$4c), ($97,$a3,$8d,$46), ($f9,$c6,$6b,$d3),
    ($5f,$e7,$03,$8f), ($9c,$95,$15,$92), ($7a,$eb,$bf,$6d), ($59,$da,$95,$52),
    ($83,$2d,$d4,$be), ($21,$d3,$58,$74), ($69,$29,$49,$e0), ($c8,$44,$8e,$c9),
    ($89,$6a,$75,$c2), ($79,$78,$f4,$8e), ($3e,$6b,$99,$58), ($71,$dd,$27,$b9), 
    ($4f,$b6,$be,$e1), ($ad,$17,$f0,$88), ($ac,$66,$c9,$20), ($3a,$b4,$7d,$ce), 
    ($4a,$18,$63,$df), ($31,$82,$e5,$1a), ($33,$60,$97,$51), ($7f,$45,$62,$53), 
    ($77,$e0,$b1,$64), ($ae,$84,$bb,$6b), ($a0,$1c,$fe,$81), ($2b,$94,$f9,$08), 
    ($68,$58,$70,$48), ($fd,$19,$8f,$45), ($6c,$87,$94,$de), ($f8,$b7,$52,$7b), 
    ($d3,$23,$ab,$73), ($02,$e2,$72,$4b), ($8f,$57,$e3,$1f), ($ab,$2a,$66,$55),
    ($28,$07,$b2,$eb), ($c2,$03,$2f,$b5), ($7b,$9a,$86,$c5), ($08,$a5,$d3,$37), 
    ($87,$f2,$30,$28), ($a5,$b2,$23,$bf), ($6a,$ba,$02,$03), ($82,$5c,$ed,$16), 
    ($1c,$2b,$8a,$cf), ($b4,$92,$a7,$79), ($f2,$f0,$f3,$07), ($e2,$a1,$4e,$69), 
    ($f4,$cd,$65,$da), ($be,$d5,$06,$05), ($62,$1f,$d1,$34), ($fe,$8a,$c4,$a6), 
    ($53,$9d,$34,$2e), ($55,$a0,$a2,$f3), ($e1,$32,$05,$8a), ($eb,$75,$a4,$f6),
    ($ec,$39,$0b,$83), ($ef,$aa,$40,$60), ($9f,$06,$5e,$71), ($10,$51,$bd,$6e),
    ($8a,$f9,$3e,$21), ($06,$3d,$96,$dd), ($05,$ae,$dd,$3e), ($bd,$46,$4d,$e6),
    ($8d,$b5,$91,$54), ($5d,$05,$71,$c4), ($d4,$6f,$04,$06), ($15,$ff,$60,$50), 
    ($fb,$24,$19,$98), ($e9,$97,$d6,$bd), ($43,$cc,$89,$40), ($9e,$77,$67,$d9), 
    ($42,$bd,$b0,$e8), ($8b,$88,$07,$89), ($5b,$38,$e7,$19), ($ee,$db,$79,$c8),
    ($0a,$47,$a1,$7c), ($0f,$e9,$7c,$42), ($1e,$c9,$f8,$84), ($00,$00,$00,$00), 
    ($86,$83,$09,$80), ($ed,$48,$32,$2b), ($70,$ac,$1e,$11), ($72,$4e,$6c,$5a), 
    ($ff,$fb,$fd,$0e), ($38,$56,$0f,$85), ($d5,$1e,$3d,$ae), ($39,$27,$36,$2d), 
    ($d9,$64,$0a,$0f), ($a6,$21,$68,$5c), ($54,$d1,$9b,$5b), ($2e,$3a,$24,$36), 
    ($67,$b1,$0c,$0a), ($e7,$0f,$93,$57), ($96,$d2,$b4,$ee), ($91,$9e,$1b,$9b), 
    ($c5,$4f,$80,$c0), ($20,$a2,$61,$dc), ($4b,$69,$5a,$77), ($1a,$16,$1c,$12), 
    ($ba,$0a,$e2,$93), ($2a,$e5,$c0,$a0), ($e0,$43,$3c,$22), ($17,$1d,$12,$1b), 
    ($0d,$0b,$0e,$09), ($c7,$ad,$f2,$8b), ($a8,$b9,$2d,$b6), ($a9,$c8,$14,$1e), 
    ($19,$85,$57,$f1), ($07,$4c,$af,$75), ($dd,$bb,$ee,$99), ($60,$fd,$a3,$7f), 
    ($26,$9f,$f7,$01), ($f5,$bc,$5c,$72), ($3b,$c5,$44,$66), ($7e,$34,$5b,$fb), 
    ($29,$76,$8b,$43), ($c6,$dc,$cb,$23), ($fc,$68,$b6,$ed), ($f1,$63,$b8,$e4), 
    ($dc,$ca,$d7,$31), ($85,$10,$42,$63), ($22,$40,$13,$97), ($11,$20,$84,$c6), 
    ($24,$7d,$85,$4a), ($3d,$f8,$d2,$bb), ($32,$11,$ae,$f9), ($a1,$6d,$c7,$29),
    ($2f,$4b,$1d,$9e), ($30,$f3,$dc,$b2), ($52,$ec,$0d,$86), ($e3,$d0,$77,$c1), 
    ($16,$6c,$2b,$b3), ($b9,$99,$a9,$70), ($48,$fa,$11,$94), ($64,$22,$47,$e9), 
    ($8c,$c4,$a8,$fc), ($3f,$1a,$a0,$f0), ($2c,$d8,$56,$7d), ($90,$ef,$22,$33), 
    ($4e,$c7,$87,$49), ($d1,$c1,$d9,$38), ($a2,$fe,$8c,$ca), ($0b,$36,$98,$d4), 
    ($81,$cf,$a6,$f5), ($de,$28,$a5,$7a), ($8e,$26,$da,$b7), ($bf,$a4,$3f,$ad),
    ($9d,$e4,$2c,$3a), ($92,$0d,$50,$78), ($cc,$9b,$6a,$5f), ($46,$62,$54,$7e),
    ($13,$c2,$f6,$8d), ($b8,$e8,$90,$d8), ($f7,$5e,$2e,$39), ($af,$f5,$82,$c3),
    ($80,$be,$9f,$5d), ($93,$7c,$69,$d0), ($2d,$a9,$6f,$d5), ($12,$b3,$cf,$25),
    ($99,$3b,$c8,$ac), ($7d,$a7,$10,$18), ($63,$6e,$e8,$9c), ($bb,$7b,$db,$3b),
    ($78,$09,$cd,$26), ($18,$f4,$6e,$59), ($b7,$01,$ec,$9a), ($9a,$a8,$83,$4f),
    ($6e,$65,$e6,$95), ($e6,$7e,$aa,$ff), ($cf,$08,$21,$bc), ($e8,$e6,$ef,$15), 
    ($9b,$d9,$ba,$e7), ($36,$ce,$4a,$6f), ($09,$d4,$ea,$9f), ($7c,$d6,$29,$b0), 
    ($b2,$af,$31,$a4), ($23,$31,$2a,$3f), ($94,$30,$c6,$a5), ($66,$c0,$35,$a2),
    ($bc,$37,$74,$4e), ($ca,$a6,$fc,$82), ($d0,$b0,$e0,$90), ($d8,$15,$33,$a7), 
    ($98,$4a,$f1,$04), ($da,$f7,$41,$ec), ($50,$0e,$7f,$cd), ($f6,$2f,$17,$91),
    ($d6,$8d,$76,$4d), ($b0,$4d,$43,$ef), ($4d,$54,$cc,$aa), ($04,$df,$e4,$96),
    ($b5,$e3,$9e,$d1), ($88,$1b,$4c,$6a), ($1f,$b8,$c1,$2c), ($51,$7f,$46,$65), 
    ($ea,$04,$9d,$5e), ($35,$5d,$01,$8c), ($74,$73,$fa,$87), ($41,$2e,$fb,$0b), 
    ($1d,$5a,$b3,$67), ($d2,$52,$92,$db), ($56,$33,$e9,$10), ($47,$13,$6d,$d6),
    ($61,$8c,$9a,$d7), ($0c,$7a,$37,$a1), ($14,$8e,$59,$f8), ($3c,$89,$eb,$13), 
    ($27,$ee,$ce,$a9), ($c9,$35,$b7,$61), ($e5,$ed,$e1,$1c), ($b1,$3c,$7a,$47),
    ($df,$59,$9c,$d2), ($73,$3f,$55,$f2), ($ce,$79,$18,$14), ($37,$bf,$73,$c7),
    ($cd,$ea,$53,$f7), ($aa,$5b,$5f,$fd), ($6f,$14,$df,$3d), ($db,$86,$78,$44),
    ($f3,$81,$ca,$af), ($c4,$3e,$b9,$68), ($34,$2c,$38,$24), ($40,$5f,$c2,$a3),
    ($c3,$72,$16,$1d), ($25,$0c,$bc,$e2), ($49,$8b,$28,$3c), ($95,$41,$ff,$0d),
    ($01,$71,$39,$a8), ($b3,$de,$08,$0c), ($e4,$9c,$d8,$b4), ($c1,$90,$64,$56),
    ($84,$61,$7b,$cb), ($b6,$70,$d5,$32), ($5c,$74,$48,$6c), ($57,$42,$d0,$b8));
  T8: array[0..255,0..3] of byte= (
    ($f4,$a7,$50,$51), ($41,$65,$53,$7e), ($17,$a4,$c3,$1a), ($27,$5e,$96,$3a),
    ($ab,$6b,$cb,$3b), ($9d,$45,$f1,$1f), ($fa,$58,$ab,$ac), ($e3,$03,$93,$4b),
    ($30,$fa,$55,$20), ($76,$6d,$f6,$ad), ($cc,$76,$91,$88), ($02,$4c,$25,$f5),
    ($e5,$d7,$fc,$4f), ($2a,$cb,$d7,$c5), ($35,$44,$80,$26), ($62,$a3,$8f,$b5), 
    ($b1,$5a,$49,$de), ($ba,$1b,$67,$25), ($ea,$0e,$98,$45), ($fe,$c0,$e1,$5d), 
    ($2f,$75,$02,$c3), ($4c,$f0,$12,$81), ($46,$97,$a3,$8d), ($d3,$f9,$c6,$6b), 
    ($8f,$5f,$e7,$03), ($92,$9c,$95,$15), ($6d,$7a,$eb,$bf), ($52,$59,$da,$95), 
    ($be,$83,$2d,$d4), ($74,$21,$d3,$58), ($e0,$69,$29,$49), ($c9,$c8,$44,$8e),
    ($c2,$89,$6a,$75), ($8e,$79,$78,$f4), ($58,$3e,$6b,$99), ($b9,$71,$dd,$27), 
    ($e1,$4f,$b6,$be), ($88,$ad,$17,$f0), ($20,$ac,$66,$c9), ($ce,$3a,$b4,$7d), 
    ($df,$4a,$18,$63), ($1a,$31,$82,$e5), ($51,$33,$60,$97), ($53,$7f,$45,$62), 
    ($64,$77,$e0,$b1), ($6b,$ae,$84,$bb), ($81,$a0,$1c,$fe), ($08,$2b,$94,$f9), 
    ($48,$68,$58,$70), ($45,$fd,$19,$8f), ($de,$6c,$87,$94), ($7b,$f8,$b7,$52), 
    ($73,$d3,$23,$ab), ($4b,$02,$e2,$72), ($1f,$8f,$57,$e3), ($55,$ab,$2a,$66), 
    ($eb,$28,$07,$b2), ($b5,$c2,$03,$2f), ($c5,$7b,$9a,$86), ($37,$08,$a5,$d3),
    ($28,$87,$f2,$30), ($bf,$a5,$b2,$23), ($03,$6a,$ba,$02), ($16,$82,$5c,$ed), 
    ($cf,$1c,$2b,$8a), ($79,$b4,$92,$a7), ($07,$f2,$f0,$f3), ($69,$e2,$a1,$4e), 
    ($da,$f4,$cd,$65), ($05,$be,$d5,$06), ($34,$62,$1f,$d1), ($a6,$fe,$8a,$c4),
    ($2e,$53,$9d,$34), ($f3,$55,$a0,$a2), ($8a,$e1,$32,$05), ($f6,$eb,$75,$a4),
    ($83,$ec,$39,$0b), ($60,$ef,$aa,$40), ($71,$9f,$06,$5e), ($6e,$10,$51,$bd), 
    ($21,$8a,$f9,$3e), ($dd,$06,$3d,$96), ($3e,$05,$ae,$dd), ($e6,$bd,$46,$4d), 
    ($54,$8d,$b5,$91), ($c4,$5d,$05,$71), ($06,$d4,$6f,$04), ($50,$15,$ff,$60),
    ($98,$fb,$24,$19), ($bd,$e9,$97,$d6), ($40,$43,$cc,$89), ($d9,$9e,$77,$67), 
    ($e8,$42,$bd,$b0), ($89,$8b,$88,$07), ($19,$5b,$38,$e7), ($c8,$ee,$db,$79),
    ($7c,$0a,$47,$a1), ($42,$0f,$e9,$7c), ($84,$1e,$c9,$f8), ($00,$00,$00,$00),
    ($80,$86,$83,$09), ($2b,$ed,$48,$32), ($11,$70,$ac,$1e), ($5a,$72,$4e,$6c), 
    ($0e,$ff,$fb,$fd), ($85,$38,$56,$0f), ($ae,$d5,$1e,$3d), ($2d,$39,$27,$36), 
    ($0f,$d9,$64,$0a), ($5c,$a6,$21,$68), ($5b,$54,$d1,$9b), ($36,$2e,$3a,$24), 
    ($0a,$67,$b1,$0c), ($57,$e7,$0f,$93), ($ee,$96,$d2,$b4), ($9b,$91,$9e,$1b),
    ($c0,$c5,$4f,$80), ($dc,$20,$a2,$61), ($77,$4b,$69,$5a), ($12,$1a,$16,$1c), 
    ($93,$ba,$0a,$e2), ($a0,$2a,$e5,$c0), ($22,$e0,$43,$3c), ($1b,$17,$1d,$12), 
    ($09,$0d,$0b,$0e), ($8b,$c7,$ad,$f2), ($b6,$a8,$b9,$2d), ($1e,$a9,$c8,$14), 
    ($f1,$19,$85,$57), ($75,$07,$4c,$af), ($99,$dd,$bb,$ee), ($7f,$60,$fd,$a3), 
    ($01,$26,$9f,$f7), ($72,$f5,$bc,$5c), ($66,$3b,$c5,$44), ($fb,$7e,$34,$5b), 
    ($43,$29,$76,$8b), ($23,$c6,$dc,$cb), ($ed,$fc,$68,$b6), ($e4,$f1,$63,$b8), 
    ($31,$dc,$ca,$d7), ($63,$85,$10,$42), ($97,$22,$40,$13), ($c6,$11,$20,$84),
    ($4a,$24,$7d,$85), ($bb,$3d,$f8,$d2), ($f9,$32,$11,$ae), ($29,$a1,$6d,$c7), 
    ($9e,$2f,$4b,$1d), ($b2,$30,$f3,$dc), ($86,$52,$ec,$0d), ($c1,$e3,$d0,$77), 
    ($b3,$16,$6c,$2b), ($70,$b9,$99,$a9), ($94,$48,$fa,$11), ($e9,$64,$22,$47), 
    ($fc,$8c,$c4,$a8), ($f0,$3f,$1a,$a0), ($7d,$2c,$d8,$56), ($33,$90,$ef,$22), 
    ($49,$4e,$c7,$87), ($38,$d1,$c1,$d9), ($ca,$a2,$fe,$8c), ($d4,$0b,$36,$98),
    ($f5,$81,$cf,$a6), ($7a,$de,$28,$a5), ($b7,$8e,$26,$da), ($ad,$bf,$a4,$3f),
    ($3a,$9d,$e4,$2c), ($78,$92,$0d,$50), ($5f,$cc,$9b,$6a), ($7e,$46,$62,$54),
    ($8d,$13,$c2,$f6), ($d8,$b8,$e8,$90), ($39,$f7,$5e,$2e), ($c3,$af,$f5,$82), 
    ($5d,$80,$be,$9f), ($d0,$93,$7c,$69), ($d5,$2d,$a9,$6f), ($25,$12,$b3,$cf),
    ($ac,$99,$3b,$c8), ($18,$7d,$a7,$10), ($9c,$63,$6e,$e8), ($3b,$bb,$7b,$db), 
    ($26,$78,$09,$cd), ($59,$18,$f4,$6e), ($9a,$b7,$01,$ec), ($4f,$9a,$a8,$83),
    ($95,$6e,$65,$e6), ($ff,$e6,$7e,$aa), ($bc,$cf,$08,$21), ($15,$e8,$e6,$ef),
    ($e7,$9b,$d9,$ba), ($6f,$36,$ce,$4a), ($9f,$09,$d4,$ea), ($b0,$7c,$d6,$29), 
    ($a4,$b2,$af,$31), ($3f,$23,$31,$2a), ($a5,$94,$30,$c6), ($a2,$66,$c0,$35),
    ($4e,$bc,$37,$74), ($82,$ca,$a6,$fc), ($90,$d0,$b0,$e0), ($a7,$d8,$15,$33),
    ($04,$98,$4a,$f1), ($ec,$da,$f7,$41), ($cd,$50,$0e,$7f), ($91,$f6,$2f,$17),
    ($4d,$d6,$8d,$76), ($ef,$b0,$4d,$43), ($aa,$4d,$54,$cc), ($96,$04,$df,$e4),
    ($d1,$b5,$e3,$9e), ($6a,$88,$1b,$4c), ($2c,$1f,$b8,$c1), ($65,$51,$7f,$46), 
    ($5e,$ea,$04,$9d), ($8c,$35,$5d,$01), ($87,$74,$73,$fa), ($0b,$41,$2e,$fb),
    ($67,$1d,$5a,$b3), ($db,$d2,$52,$92), ($10,$56,$33,$e9), ($d6,$47,$13,$6d),
    ($d7,$61,$8c,$9a), ($a1,$0c,$7a,$37), ($f8,$14,$8e,$59), ($13,$3c,$89,$eb), 
    ($a9,$27,$ee,$ce), ($61,$c9,$35,$b7), ($1c,$e5,$ed,$e1), ($47,$b1,$3c,$7a),
    ($d2,$df,$59,$9c), ($f2,$73,$3f,$55), ($14,$ce,$79,$18), ($c7,$37,$bf,$73),
    ($f7,$cd,$ea,$53), ($fd,$aa,$5b,$5f), ($3d,$6f,$14,$df), ($44,$db,$86,$78),
    ($af,$f3,$81,$ca), ($68,$c4,$3e,$b9), ($24,$34,$2c,$38), ($a3,$40,$5f,$c2),
    ($1d,$c3,$72,$16), ($e2,$25,$0c,$bc), ($3c,$49,$8b,$28), ($0d,$95,$41,$ff),
    ($a8,$01,$71,$39), ($0c,$b3,$de,$08), ($b4,$e4,$9c,$d8), ($56,$c1,$90,$64),
    ($cb,$84,$61,$7b), ($32,$b6,$70,$d5), ($6c,$5c,$74,$48), ($b8,$57,$42,$d0));
  S5: array[0..255] of byte= (
    $52,$09,$6a,$d5,
    $30,$36,$a5,$38,
    $bf,$40,$a3,$9e,
    $81,$f3,$d7,$fb,
    $7c,$e3,$39,$82,
    $9b,$2f,$ff,$87,
    $34,$8e,$43,$44,
    $c4,$de,$e9,$cb,
    $54,$7b,$94,$32,
    $a6,$c2,$23,$3d,
    $ee,$4c,$95,$0b,
    $42,$fa,$c3,$4e,
    $08,$2e,$a1,$66,
    $28,$d9,$24,$b2,
    $76,$5b,$a2,$49,
    $6d,$8b,$d1,$25,
    $72,$f8,$f6,$64,
    $86,$68,$98,$16,
    $d4,$a4,$5c,$cc,
    $5d,$65,$b6,$92,
    $6c,$70,$48,$50,
    $fd,$ed,$b9,$da,
    $5e,$15,$46,$57,
    $a7,$8d,$9d,$84,
    $90,$d8,$ab,$00,
    $8c,$bc,$d3,$0a,
    $f7,$e4,$58,$05,
    $b8,$b3,$45,$06,
    $d0,$2c,$1e,$8f,
    $ca,$3f,$0f,$02,
    $c1,$af,$bd,$03,
    $01,$13,$8a,$6b,
    $3a,$91,$11,$41,
    $4f,$67,$dc,$ea,
    $97,$f2,$cf,$ce,
    $f0,$b4,$e6,$73,
    $96,$ac,$74,$22,
    $e7,$ad,$35,$85,
    $e2,$f9,$37,$e8,
    $1c,$75,$df,$6e,
    $47,$f1,$1a,$71,
    $1d,$29,$c5,$89,
    $6f,$b7,$62,$0e,
    $aa,$18,$be,$1b,
    $fc,$56,$3e,$4b,
    $c6,$d2,$79,$20,
    $9a,$db,$c0,$fe,
    $78,$cd,$5a,$f4,
    $1f,$dd,$a8,$33,
    $88,$07,$c7,$31,
    $b1,$12,$10,$59,
    $27,$80,$ec,$5f,
    $60,$51,$7f,$a9,
    $19,$b5,$4a,$0d,
    $2d,$e5,$7a,$9f,
    $93,$c9,$9c,$ef,
    $a0,$e0,$3b,$4d,
    $ae,$2a,$f5,$b0,
    $c8,$eb,$bb,$3c,
    $83,$53,$99,$61,
    $17,$2b,$04,$7e,
    $ba,$77,$d6,$26,
    $e1,$69,$14,$63,
    $55,$21,$0c,$7d);
  U1: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($0e,$09,$0d,$0b), ($1c,$12,$1a,$16), ($12,$1b,$17,$1d),
    ($38,$24,$34,$2c), ($36,$2d,$39,$27), ($24,$36,$2e,$3a), ($2a,$3f,$23,$31),
    ($70,$48,$68,$58), ($7e,$41,$65,$53), ($6c,$5a,$72,$4e), ($62,$53,$7f,$45),
    ($48,$6c,$5c,$74), ($46,$65,$51,$7f), ($54,$7e,$46,$62), ($5a,$77,$4b,$69),
    ($e0,$90,$d0,$b0), ($ee,$99,$dd,$bb), ($fc,$82,$ca,$a6), ($f2,$8b,$c7,$ad), 
    ($d8,$b4,$e4,$9c), ($d6,$bd,$e9,$97), ($c4,$a6,$fe,$8a), ($ca,$af,$f3,$81), 
    ($90,$d8,$b8,$e8), ($9e,$d1,$b5,$e3), ($8c,$ca,$a2,$fe), ($82,$c3,$af,$f5),
    ($a8,$fc,$8c,$c4), ($a6,$f5,$81,$cf), ($b4,$ee,$96,$d2), ($ba,$e7,$9b,$d9),
    ($db,$3b,$bb,$7b), ($d5,$32,$b6,$70), ($c7,$29,$a1,$6d), ($c9,$20,$ac,$66),
    ($e3,$1f,$8f,$57), ($ed,$16,$82,$5c), ($ff,$0d,$95,$41), ($f1,$04,$98,$4a),
    ($ab,$73,$d3,$23), ($a5,$7a,$de,$28), ($b7,$61,$c9,$35), ($b9,$68,$c4,$3e),
    ($93,$57,$e7,$0f), ($9d,$5e,$ea,$04), ($8f,$45,$fd,$19), ($81,$4c,$f0,$12),
    ($3b,$ab,$6b,$cb), ($35,$a2,$66,$c0), ($27,$b9,$71,$dd), ($29,$b0,$7c,$d6),
    ($03,$8f,$5f,$e7), ($0d,$86,$52,$ec), ($1f,$9d,$45,$f1), ($11,$94,$48,$fa),
    ($4b,$e3,$03,$93), ($45,$ea,$0e,$98), ($57,$f1,$19,$85), ($59,$f8,$14,$8e),
    ($73,$c7,$37,$bf), ($7d,$ce,$3a,$b4), ($6f,$d5,$2d,$a9), ($61,$dc,$20,$a2),
    ($ad,$76,$6d,$f6), ($a3,$7f,$60,$fd), ($b1,$64,$77,$e0), ($bf,$6d,$7a,$eb),
    ($95,$52,$59,$da), ($9b,$5b,$54,$d1), ($89,$40,$43,$cc), ($87,$49,$4e,$c7),
    ($dd,$3e,$05,$ae), ($d3,$37,$08,$a5), ($c1,$2c,$1f,$b8), ($cf,$25,$12,$b3),
    ($e5,$1a,$31,$82), ($eb,$13,$3c,$89), ($f9,$08,$2b,$94), ($f7,$01,$26,$9f),
    ($4d,$e6,$bd,$46), ($43,$ef,$b0,$4d), ($51,$f4,$a7,$50), ($5f,$fd,$aa,$5b),
    ($75,$c2,$89,$6a), ($7b,$cb,$84,$61), ($69,$d0,$93,$7c), ($67,$d9,$9e,$77), 
    ($3d,$ae,$d5,$1e), ($33,$a7,$d8,$15), ($21,$bc,$cf,$08), ($2f,$b5,$c2,$03),
    ($05,$8a,$e1,$32), ($0b,$83,$ec,$39), ($19,$98,$fb,$24), ($17,$91,$f6,$2f),
    ($76,$4d,$d6,$8d), ($78,$44,$db,$86), ($6a,$5f,$cc,$9b), ($64,$56,$c1,$90), 
    ($4e,$69,$e2,$a1), ($40,$60,$ef,$aa), ($52,$7b,$f8,$b7), ($5c,$72,$f5,$bc),
    ($06,$05,$be,$d5), ($08,$0c,$b3,$de), ($1a,$17,$a4,$c3), ($14,$1e,$a9,$c8), 
    ($3e,$21,$8a,$f9), ($30,$28,$87,$f2), ($22,$33,$90,$ef), ($2c,$3a,$9d,$e4), 
    ($96,$dd,$06,$3d), ($98,$d4,$0b,$36), ($8a,$cf,$1c,$2b), ($84,$c6,$11,$20), 
    ($ae,$f9,$32,$11), ($a0,$f0,$3f,$1a), ($b2,$eb,$28,$07), ($bc,$e2,$25,$0c),
    ($e6,$95,$6e,$65), ($e8,$9c,$63,$6e), ($fa,$87,$74,$73), ($f4,$8e,$79,$78), 
    ($de,$b1,$5a,$49), ($d0,$b8,$57,$42), ($c2,$a3,$40,$5f), ($cc,$aa,$4d,$54),
    ($41,$ec,$da,$f7), ($4f,$e5,$d7,$fc), ($5d,$fe,$c0,$e1), ($53,$f7,$cd,$ea),
    ($79,$c8,$ee,$db), ($77,$c1,$e3,$d0), ($65,$da,$f4,$cd), ($6b,$d3,$f9,$c6), 
    ($31,$a4,$b2,$af), ($3f,$ad,$bf,$a4), ($2d,$b6,$a8,$b9), ($23,$bf,$a5,$b2), 
    ($09,$80,$86,$83), ($07,$89,$8b,$88), ($15,$92,$9c,$95), ($1b,$9b,$91,$9e), 
    ($a1,$7c,$0a,$47), ($af,$75,$07,$4c), ($bd,$6e,$10,$51), ($b3,$67,$1d,$5a), 
    ($99,$58,$3e,$6b), ($97,$51,$33,$60), ($85,$4a,$24,$7d), ($8b,$43,$29,$76), 
    ($d1,$34,$62,$1f), ($df,$3d,$6f,$14), ($cd,$26,$78,$09), ($c3,$2f,$75,$02), 
    ($e9,$10,$56,$33), ($e7,$19,$5b,$38), ($f5,$02,$4c,$25), ($fb,$0b,$41,$2e), 
    ($9a,$d7,$61,$8c), ($94,$de,$6c,$87), ($86,$c5,$7b,$9a), ($88,$cc,$76,$91),
    ($a2,$f3,$55,$a0), ($ac,$fa,$58,$ab), ($be,$e1,$4f,$b6), ($b0,$e8,$42,$bd),
    ($ea,$9f,$09,$d4), ($e4,$96,$04,$df), ($f6,$8d,$13,$c2), ($f8,$84,$1e,$c9), 
    ($d2,$bb,$3d,$f8), ($dc,$b2,$30,$f3), ($ce,$a9,$27,$ee), ($c0,$a0,$2a,$e5), 
    ($7a,$47,$b1,$3c), ($74,$4e,$bc,$37), ($66,$55,$ab,$2a), ($68,$5c,$a6,$21), 
    ($42,$63,$85,$10), ($4c,$6a,$88,$1b), ($5e,$71,$9f,$06), ($50,$78,$92,$0d),
    ($0a,$0f,$d9,$64), ($04,$06,$d4,$6f), ($16,$1d,$c3,$72), ($18,$14,$ce,$79),
    ($32,$2b,$ed,$48), ($3c,$22,$e0,$43), ($2e,$39,$f7,$5e), ($20,$30,$fa,$55),
    ($ec,$9a,$b7,$01), ($e2,$93,$ba,$0a), ($f0,$88,$ad,$17), ($fe,$81,$a0,$1c),
    ($d4,$be,$83,$2d), ($da,$b7,$8e,$26), ($c8,$ac,$99,$3b), ($c6,$a5,$94,$30), 
    ($9c,$d2,$df,$59), ($92,$db,$d2,$52), ($80,$c0,$c5,$4f), ($8e,$c9,$c8,$44), 
    ($a4,$f6,$eb,$75), ($aa,$ff,$e6,$7e), ($b8,$e4,$f1,$63), ($b6,$ed,$fc,$68), 
    ($0c,$0a,$67,$b1), ($02,$03,$6a,$ba), ($10,$18,$7d,$a7), ($1e,$11,$70,$ac),
    ($34,$2e,$53,$9d), ($3a,$27,$5e,$96), ($28,$3c,$49,$8b), ($26,$35,$44,$80), 
    ($7c,$42,$0f,$e9), ($72,$4b,$02,$e2), ($60,$50,$15,$ff), ($6e,$59,$18,$f4),
    ($44,$66,$3b,$c5), ($4a,$6f,$36,$ce), ($58,$74,$21,$d3), ($56,$7d,$2c,$d8),
    ($37,$a1,$0c,$7a), ($39,$a8,$01,$71), ($2b,$b3,$16,$6c), ($25,$ba,$1b,$67), 
    ($0f,$85,$38,$56), ($01,$8c,$35,$5d), ($13,$97,$22,$40), ($1d,$9e,$2f,$4b), 
    ($47,$e9,$64,$22), ($49,$e0,$69,$29), ($5b,$fb,$7e,$34), ($55,$f2,$73,$3f),
    ($7f,$cd,$50,$0e), ($71,$c4,$5d,$05), ($63,$df,$4a,$18), ($6d,$d6,$47,$13),
    ($d7,$31,$dc,$ca), ($d9,$38,$d1,$c1), ($cb,$23,$c6,$dc), ($c5,$2a,$cb,$d7),
    ($ef,$15,$e8,$e6), ($e1,$1c,$e5,$ed), ($f3,$07,$f2,$f0), ($fd,$0e,$ff,$fb),
    ($a7,$79,$b4,$92), ($a9,$70,$b9,$99), ($bb,$6b,$ae,$84), ($b5,$62,$a3,$8f),
    ($9f,$5d,$80,$be), ($91,$54,$8d,$b5), ($83,$4f,$9a,$a8), ($8d,$46,$97,$a3));
  U2: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($0b,$0e,$09,$0d), ($16,$1c,$12,$1a), ($1d,$12,$1b,$17),
    ($2c,$38,$24,$34), ($27,$36,$2d,$39), ($3a,$24,$36,$2e), ($31,$2a,$3f,$23),
    ($58,$70,$48,$68), ($53,$7e,$41,$65), ($4e,$6c,$5a,$72), ($45,$62,$53,$7f),
    ($74,$48,$6c,$5c), ($7f,$46,$65,$51), ($62,$54,$7e,$46), ($69,$5a,$77,$4b),
    ($b0,$e0,$90,$d0), ($bb,$ee,$99,$dd), ($a6,$fc,$82,$ca), ($ad,$f2,$8b,$c7),
    ($9c,$d8,$b4,$e4), ($97,$d6,$bd,$e9), ($8a,$c4,$a6,$fe), ($81,$ca,$af,$f3), 
    ($e8,$90,$d8,$b8), ($e3,$9e,$d1,$b5), ($fe,$8c,$ca,$a2), ($f5,$82,$c3,$af), 
    ($c4,$a8,$fc,$8c), ($cf,$a6,$f5,$81), ($d2,$b4,$ee,$96), ($d9,$ba,$e7,$9b), 
    ($7b,$db,$3b,$bb), ($70,$d5,$32,$b6), ($6d,$c7,$29,$a1), ($66,$c9,$20,$ac),
    ($57,$e3,$1f,$8f), ($5c,$ed,$16,$82), ($41,$ff,$0d,$95), ($4a,$f1,$04,$98), 
    ($23,$ab,$73,$d3), ($28,$a5,$7a,$de), ($35,$b7,$61,$c9), ($3e,$b9,$68,$c4), 
    ($0f,$93,$57,$e7), ($04,$9d,$5e,$ea), ($19,$8f,$45,$fd), ($12,$81,$4c,$f0), 
    ($cb,$3b,$ab,$6b), ($c0,$35,$a2,$66), ($dd,$27,$b9,$71), ($d6,$29,$b0,$7c),
    ($e7,$03,$8f,$5f), ($ec,$0d,$86,$52), ($f1,$1f,$9d,$45), ($fa,$11,$94,$48),
    ($93,$4b,$e3,$03), ($98,$45,$ea,$0e), ($85,$57,$f1,$19), ($8e,$59,$f8,$14), 
    ($bf,$73,$c7,$37), ($b4,$7d,$ce,$3a), ($a9,$6f,$d5,$2d), ($a2,$61,$dc,$20), 
    ($f6,$ad,$76,$6d), ($fd,$a3,$7f,$60), ($e0,$b1,$64,$77), ($eb,$bf,$6d,$7a),
    ($da,$95,$52,$59), ($d1,$9b,$5b,$54), ($cc,$89,$40,$43), ($c7,$87,$49,$4e), 
    ($ae,$dd,$3e,$05), ($a5,$d3,$37,$08), ($b8,$c1,$2c,$1f), ($b3,$cf,$25,$12),
    ($82,$e5,$1a,$31), ($89,$eb,$13,$3c), ($94,$f9,$08,$2b), ($9f,$f7,$01,$26), 
    ($46,$4d,$e6,$bd), ($4d,$43,$ef,$b0), ($50,$51,$f4,$a7), ($5b,$5f,$fd,$aa), 
    ($6a,$75,$c2,$89), ($61,$7b,$cb,$84), ($7c,$69,$d0,$93), ($77,$67,$d9,$9e), 
    ($1e,$3d,$ae,$d5), ($15,$33,$a7,$d8), ($08,$21,$bc,$cf), ($03,$2f,$b5,$c2), 
    ($32,$05,$8a,$e1), ($39,$0b,$83,$ec), ($24,$19,$98,$fb), ($2f,$17,$91,$f6),
    ($8d,$76,$4d,$d6), ($86,$78,$44,$db), ($9b,$6a,$5f,$cc), ($90,$64,$56,$c1),
    ($a1,$4e,$69,$e2), ($aa,$40,$60,$ef), ($b7,$52,$7b,$f8), ($bc,$5c,$72,$f5), 
    ($d5,$06,$05,$be), ($de,$08,$0c,$b3), ($c3,$1a,$17,$a4), ($c8,$14,$1e,$a9), 
    ($f9,$3e,$21,$8a), ($f2,$30,$28,$87), ($ef,$22,$33,$90), ($e4,$2c,$3a,$9d),
    ($3d,$96,$dd,$06), ($36,$98,$d4,$0b), ($2b,$8a,$cf,$1c), ($20,$84,$c6,$11), 
    ($11,$ae,$f9,$32), ($1a,$a0,$f0,$3f), ($07,$b2,$eb,$28), ($0c,$bc,$e2,$25), 
    ($65,$e6,$95,$6e), ($6e,$e8,$9c,$63), ($73,$fa,$87,$74), ($78,$f4,$8e,$79), 
    ($49,$de,$b1,$5a), ($42,$d0,$b8,$57), ($5f,$c2,$a3,$40), ($54,$cc,$aa,$4d),
    ($f7,$41,$ec,$da), ($fc,$4f,$e5,$d7), ($e1,$5d,$fe,$c0), ($ea,$53,$f7,$cd), 
    ($db,$79,$c8,$ee), ($d0,$77,$c1,$e3), ($cd,$65,$da,$f4), ($c6,$6b,$d3,$f9), 
    ($af,$31,$a4,$b2), ($a4,$3f,$ad,$bf), ($b9,$2d,$b6,$a8), ($b2,$23,$bf,$a5), 
    ($83,$09,$80,$86), ($88,$07,$89,$8b), ($95,$15,$92,$9c), ($9e,$1b,$9b,$91),
    ($47,$a1,$7c,$0a), ($4c,$af,$75,$07), ($51,$bd,$6e,$10), ($5a,$b3,$67,$1d),
    ($6b,$99,$58,$3e), ($60,$97,$51,$33), ($7d,$85,$4a,$24), ($76,$8b,$43,$29), 
    ($1f,$d1,$34,$62), ($14,$df,$3d,$6f), ($09,$cd,$26,$78), ($02,$c3,$2f,$75), 
    ($33,$e9,$10,$56), ($38,$e7,$19,$5b), ($25,$f5,$02,$4c), ($2e,$fb,$0b,$41),
    ($8c,$9a,$d7,$61), ($87,$94,$de,$6c), ($9a,$86,$c5,$7b), ($91,$88,$cc,$76), 
    ($a0,$a2,$f3,$55), ($ab,$ac,$fa,$58), ($b6,$be,$e1,$4f), ($bd,$b0,$e8,$42),
    ($d4,$ea,$9f,$09), ($df,$e4,$96,$04), ($c2,$f6,$8d,$13), ($c9,$f8,$84,$1e), 
    ($f8,$d2,$bb,$3d), ($f3,$dc,$b2,$30), ($ee,$ce,$a9,$27), ($e5,$c0,$a0,$2a), 
    ($3c,$7a,$47,$b1), ($37,$74,$4e,$bc), ($2a,$66,$55,$ab), ($21,$68,$5c,$a6),
    ($10,$42,$63,$85), ($1b,$4c,$6a,$88), ($06,$5e,$71,$9f), ($0d,$50,$78,$92), 
    ($64,$0a,$0f,$d9), ($6f,$04,$06,$d4), ($72,$16,$1d,$c3), ($79,$18,$14,$ce),
    ($48,$32,$2b,$ed), ($43,$3c,$22,$e0), ($5e,$2e,$39,$f7), ($55,$20,$30,$fa),
    ($01,$ec,$9a,$b7), ($0a,$e2,$93,$ba), ($17,$f0,$88,$ad), ($1c,$fe,$81,$a0), 
    ($2d,$d4,$be,$83), ($26,$da,$b7,$8e), ($3b,$c8,$ac,$99), ($30,$c6,$a5,$94), 
    ($59,$9c,$d2,$df), ($52,$92,$db,$d2), ($4f,$80,$c0,$c5), ($44,$8e,$c9,$c8),
    ($75,$a4,$f6,$eb), ($7e,$aa,$ff,$e6), ($63,$b8,$e4,$f1), ($68,$b6,$ed,$fc), 
    ($b1,$0c,$0a,$67), ($ba,$02,$03,$6a), ($a7,$10,$18,$7d), ($ac,$1e,$11,$70), 
    ($9d,$34,$2e,$53), ($96,$3a,$27,$5e), ($8b,$28,$3c,$49), ($80,$26,$35,$44), 
    ($e9,$7c,$42,$0f), ($e2,$72,$4b,$02), ($ff,$60,$50,$15), ($f4,$6e,$59,$18),
    ($c5,$44,$66,$3b), ($ce,$4a,$6f,$36), ($d3,$58,$74,$21), ($d8,$56,$7d,$2c), 
    ($7a,$37,$a1,$0c), ($71,$39,$a8,$01), ($6c,$2b,$b3,$16), ($67,$25,$ba,$1b), 
    ($56,$0f,$85,$38), ($5d,$01,$8c,$35), ($40,$13,$97,$22), ($4b,$1d,$9e,$2f), 
    ($22,$47,$e9,$64), ($29,$49,$e0,$69), ($34,$5b,$fb,$7e), ($3f,$55,$f2,$73),
    ($0e,$7f,$cd,$50), ($05,$71,$c4,$5d), ($18,$63,$df,$4a), ($13,$6d,$d6,$47),
    ($ca,$d7,$31,$dc), ($c1,$d9,$38,$d1), ($dc,$cb,$23,$c6), ($d7,$c5,$2a,$cb), 
    ($e6,$ef,$15,$e8), ($ed,$e1,$1c,$e5), ($f0,$f3,$07,$f2), ($fb,$fd,$0e,$ff), 
    ($92,$a7,$79,$b4), ($99,$a9,$70,$b9), ($84,$bb,$6b,$ae), ($8f,$b5,$62,$a3),
    ($be,$9f,$5d,$80), ($b5,$91,$54,$8d), ($a8,$83,$4f,$9a), ($a3,$8d,$46,$97));
  U3: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($0d,$0b,$0e,$09), ($1a,$16,$1c,$12), ($17,$1d,$12,$1b),
    ($34,$2c,$38,$24), ($39,$27,$36,$2d), ($2e,$3a,$24,$36), ($23,$31,$2a,$3f),
    ($68,$58,$70,$48), ($65,$53,$7e,$41), ($72,$4e,$6c,$5a), ($7f,$45,$62,$53),
    ($5c,$74,$48,$6c), ($51,$7f,$46,$65), ($46,$62,$54,$7e), ($4b,$69,$5a,$77),
    ($d0,$b0,$e0,$90), ($dd,$bb,$ee,$99), ($ca,$a6,$fc,$82), ($c7,$ad,$f2,$8b), 
    ($e4,$9c,$d8,$b4), ($e9,$97,$d6,$bd), ($fe,$8a,$c4,$a6), ($f3,$81,$ca,$af), 
    ($b8,$e8,$90,$d8), ($b5,$e3,$9e,$d1), ($a2,$fe,$8c,$ca), ($af,$f5,$82,$c3),
    ($8c,$c4,$a8,$fc), ($81,$cf,$a6,$f5), ($96,$d2,$b4,$ee), ($9b,$d9,$ba,$e7), 
    ($bb,$7b,$db,$3b), ($b6,$70,$d5,$32), ($a1,$6d,$c7,$29), ($ac,$66,$c9,$20), 
    ($8f,$57,$e3,$1f), ($82,$5c,$ed,$16), ($95,$41,$ff,$0d), ($98,$4a,$f1,$04), 
    ($d3,$23,$ab,$73), ($de,$28,$a5,$7a), ($c9,$35,$b7,$61), ($c4,$3e,$b9,$68),
    ($e7,$0f,$93,$57), ($ea,$04,$9d,$5e), ($fd,$19,$8f,$45), ($f0,$12,$81,$4c), 
    ($6b,$cb,$3b,$ab), ($66,$c0,$35,$a2), ($71,$dd,$27,$b9), ($7c,$d6,$29,$b0), 
    ($5f,$e7,$03,$8f), ($52,$ec,$0d,$86), ($45,$f1,$1f,$9d), ($48,$fa,$11,$94), 
    ($03,$93,$4b,$e3), ($0e,$98,$45,$ea), ($19,$85,$57,$f1), ($14,$8e,$59,$f8), 
    ($37,$bf,$73,$c7), ($3a,$b4,$7d,$ce), ($2d,$a9,$6f,$d5), ($20,$a2,$61,$dc), 
    ($6d,$f6,$ad,$76), ($60,$fd,$a3,$7f), ($77,$e0,$b1,$64), ($7a,$eb,$bf,$6d),
    ($59,$da,$95,$52), ($54,$d1,$9b,$5b), ($43,$cc,$89,$40), ($4e,$c7,$87,$49),
    ($05,$ae,$dd,$3e), ($08,$a5,$d3,$37), ($1f,$b8,$c1,$2c), ($12,$b3,$cf,$25),
    ($31,$82,$e5,$1a), ($3c,$89,$eb,$13), ($2b,$94,$f9,$08), ($26,$9f,$f7,$01),
    ($bd,$46,$4d,$e6), ($b0,$4d,$43,$ef), ($a7,$50,$51,$f4), ($aa,$5b,$5f,$fd), 
    ($89,$6a,$75,$c2), ($84,$61,$7b,$cb), ($93,$7c,$69,$d0), ($9e,$77,$67,$d9), 
    ($d5,$1e,$3d,$ae), ($d8,$15,$33,$a7), ($cf,$08,$21,$bc), ($c2,$03,$2f,$b5),
    ($e1,$32,$05,$8a), ($ec,$39,$0b,$83), ($fb,$24,$19,$98), ($f6,$2f,$17,$91),
    ($d6,$8d,$76,$4d), ($db,$86,$78,$44), ($cc,$9b,$6a,$5f), ($c1,$90,$64,$56), 
    ($e2,$a1,$4e,$69), ($ef,$aa,$40,$60), ($f8,$b7,$52,$7b), ($f5,$bc,$5c,$72),
    ($be,$d5,$06,$05), ($b3,$de,$08,$0c), ($a4,$c3,$1a,$17), ($a9,$c8,$14,$1e),
    ($8a,$f9,$3e,$21), ($87,$f2,$30,$28), ($90,$ef,$22,$33), ($9d,$e4,$2c,$3a), 
    ($06,$3d,$96,$dd), ($0b,$36,$98,$d4), ($1c,$2b,$8a,$cf), ($11,$20,$84,$c6), 
    ($32,$11,$ae,$f9), ($3f,$1a,$a0,$f0), ($28,$07,$b2,$eb), ($25,$0c,$bc,$e2),
    ($6e,$65,$e6,$95), ($63,$6e,$e8,$9c), ($74,$73,$fa,$87), ($79,$78,$f4,$8e), 
    ($5a,$49,$de,$b1), ($57,$42,$d0,$b8), ($40,$5f,$c2,$a3), ($4d,$54,$cc,$aa), 
    ($da,$f7,$41,$ec), ($d7,$fc,$4f,$e5), ($c0,$e1,$5d,$fe), ($cd,$ea,$53,$f7), 
    ($ee,$db,$79,$c8), ($e3,$d0,$77,$c1), ($f4,$cd,$65,$da), ($f9,$c6,$6b,$d3),
    ($b2,$af,$31,$a4), ($bf,$a4,$3f,$ad), ($a8,$b9,$2d,$b6), ($a5,$b2,$23,$bf), 
    ($86,$83,$09,$80), ($8b,$88,$07,$89), ($9c,$95,$15,$92), ($91,$9e,$1b,$9b), 
    ($0a,$47,$a1,$7c), ($07,$4c,$af,$75), ($10,$51,$bd,$6e), ($1d,$5a,$b3,$67), 
    ($3e,$6b,$99,$58), ($33,$60,$97,$51), ($24,$7d,$85,$4a), ($29,$76,$8b,$43), 
    ($62,$1f,$d1,$34), ($6f,$14,$df,$3d), ($78,$09,$cd,$26), ($75,$02,$c3,$2f), 
    ($56,$33,$e9,$10), ($5b,$38,$e7,$19), ($4c,$25,$f5,$02), ($41,$2e,$fb,$0b),
    ($61,$8c,$9a,$d7), ($6c,$87,$94,$de), ($7b,$9a,$86,$c5), ($76,$91,$88,$cc),
    ($55,$a0,$a2,$f3), ($58,$ab,$ac,$fa), ($4f,$b6,$be,$e1), ($42,$bd,$b0,$e8),
    ($09,$d4,$ea,$9f), ($04,$df,$e4,$96), ($13,$c2,$f6,$8d), ($1e,$c9,$f8,$84),
    ($3d,$f8,$d2,$bb), ($30,$f3,$dc,$b2), ($27,$ee,$ce,$a9), ($2a,$e5,$c0,$a0), 
    ($b1,$3c,$7a,$47), ($bc,$37,$74,$4e), ($ab,$2a,$66,$55), ($a6,$21,$68,$5c), 
    ($85,$10,$42,$63), ($88,$1b,$4c,$6a), ($9f,$06,$5e,$71), ($92,$0d,$50,$78),
    ($d9,$64,$0a,$0f), ($d4,$6f,$04,$06), ($c3,$72,$16,$1d), ($ce,$79,$18,$14),
    ($ed,$48,$32,$2b), ($e0,$43,$3c,$22), ($f7,$5e,$2e,$39), ($fa,$55,$20,$30),
    ($b7,$01,$ec,$9a), ($ba,$0a,$e2,$93), ($ad,$17,$f0,$88), ($a0,$1c,$fe,$81),
    ($83,$2d,$d4,$be), ($8e,$26,$da,$b7), ($99,$3b,$c8,$ac), ($94,$30,$c6,$a5),
    ($df,$59,$9c,$d2), ($d2,$52,$92,$db), ($c5,$4f,$80,$c0), ($c8,$44,$8e,$c9),
    ($eb,$75,$a4,$f6), ($e6,$7e,$aa,$ff), ($f1,$63,$b8,$e4), ($fc,$68,$b6,$ed),
    ($67,$b1,$0c,$0a), ($6a,$ba,$02,$03), ($7d,$a7,$10,$18), ($70,$ac,$1e,$11),
    ($53,$9d,$34,$2e), ($5e,$96,$3a,$27), ($49,$8b,$28,$3c), ($44,$80,$26,$35),
    ($0f,$e9,$7c,$42), ($02,$e2,$72,$4b), ($15,$ff,$60,$50), ($18,$f4,$6e,$59),
    ($3b,$c5,$44,$66), ($36,$ce,$4a,$6f), ($21,$d3,$58,$74), ($2c,$d8,$56,$7d),
    ($0c,$7a,$37,$a1), ($01,$71,$39,$a8), ($16,$6c,$2b,$b3), ($1b,$67,$25,$ba),
    ($38,$56,$0f,$85), ($35,$5d,$01,$8c), ($22,$40,$13,$97), ($2f,$4b,$1d,$9e),
    ($64,$22,$47,$e9), ($69,$29,$49,$e0), ($7e,$34,$5b,$fb), ($73,$3f,$55,$f2),
    ($50,$0e,$7f,$cd), ($5d,$05,$71,$c4), ($4a,$18,$63,$df), ($47,$13,$6d,$d6),
    ($dc,$ca,$d7,$31), ($d1,$c1,$d9,$38), ($c6,$dc,$cb,$23), ($cb,$d7,$c5,$2a),
    ($e8,$e6,$ef,$15), ($e5,$ed,$e1,$1c), ($f2,$f0,$f3,$07), ($ff,$fb,$fd,$0e),
    ($b4,$92,$a7,$79), ($b9,$99,$a9,$70), ($ae,$84,$bb,$6b), ($a3,$8f,$b5,$62),
    ($80,$be,$9f,$5d), ($8d,$b5,$91,$54), ($9a,$a8,$83,$4f), ($97,$a3,$8d,$46));
  U4: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($09,$0d,$0b,$0e), ($12,$1a,$16,$1c), ($1b,$17,$1d,$12),
    ($24,$34,$2c,$38), ($2d,$39,$27,$36), ($36,$2e,$3a,$24), ($3f,$23,$31,$2a),
    ($48,$68,$58,$70), ($41,$65,$53,$7e), ($5a,$72,$4e,$6c), ($53,$7f,$45,$62),
    ($6c,$5c,$74,$48), ($65,$51,$7f,$46), ($7e,$46,$62,$54), ($77,$4b,$69,$5a),
    ($90,$d0,$b0,$e0), ($99,$dd,$bb,$ee), ($82,$ca,$a6,$fc), ($8b,$c7,$ad,$f2),
    ($b4,$e4,$9c,$d8), ($bd,$e9,$97,$d6), ($a6,$fe,$8a,$c4), ($af,$f3,$81,$ca),
    ($d8,$b8,$e8,$90), ($d1,$b5,$e3,$9e), ($ca,$a2,$fe,$8c), ($c3,$af,$f5,$82),
    ($fc,$8c,$c4,$a8), ($f5,$81,$cf,$a6), ($ee,$96,$d2,$b4), ($e7,$9b,$d9,$ba), 
    ($3b,$bb,$7b,$db), ($32,$b6,$70,$d5), ($29,$a1,$6d,$c7), ($20,$ac,$66,$c9),
    ($1f,$8f,$57,$e3), ($16,$82,$5c,$ed), ($0d,$95,$41,$ff), ($04,$98,$4a,$f1), 
    ($73,$d3,$23,$ab), ($7a,$de,$28,$a5), ($61,$c9,$35,$b7), ($68,$c4,$3e,$b9), 
    ($57,$e7,$0f,$93), ($5e,$ea,$04,$9d), ($45,$fd,$19,$8f), ($4c,$f0,$12,$81), 
    ($ab,$6b,$cb,$3b), ($a2,$66,$c0,$35), ($b9,$71,$dd,$27), ($b0,$7c,$d6,$29),
    ($8f,$5f,$e7,$03), ($86,$52,$ec,$0d), ($9d,$45,$f1,$1f), ($94,$48,$fa,$11), 
    ($e3,$03,$93,$4b), ($ea,$0e,$98,$45), ($f1,$19,$85,$57), ($f8,$14,$8e,$59), 
    ($c7,$37,$bf,$73), ($ce,$3a,$b4,$7d), ($d5,$2d,$a9,$6f), ($dc,$20,$a2,$61), 
    ($76,$6d,$f6,$ad), ($7f,$60,$fd,$a3), ($64,$77,$e0,$b1), ($6d,$7a,$eb,$bf), 
    ($52,$59,$da,$95), ($5b,$54,$d1,$9b), ($40,$43,$cc,$89), ($49,$4e,$c7,$87),
    ($3e,$05,$ae,$dd), ($37,$08,$a5,$d3), ($2c,$1f,$b8,$c1), ($25,$12,$b3,$cf),
    ($1a,$31,$82,$e5), ($13,$3c,$89,$eb), ($08,$2b,$94,$f9), ($01,$26,$9f,$f7),
    ($e6,$bd,$46,$4d), ($ef,$b0,$4d,$43), ($f4,$a7,$50,$51), ($fd,$aa,$5b,$5f),
    ($c2,$89,$6a,$75), ($cb,$84,$61,$7b), ($d0,$93,$7c,$69), ($d9,$9e,$77,$67),
    ($ae,$d5,$1e,$3d), ($a7,$d8,$15,$33), ($bc,$cf,$08,$21), ($b5,$c2,$03,$2f), 
    ($8a,$e1,$32,$05), ($83,$ec,$39,$0b), ($98,$fb,$24,$19), ($91,$f6,$2f,$17),
    ($4d,$d6,$8d,$76), ($44,$db,$86,$78), ($5f,$cc,$9b,$6a), ($56,$c1,$90,$64), 
    ($69,$e2,$a1,$4e), ($60,$ef,$aa,$40), ($7b,$f8,$b7,$52), ($72,$f5,$bc,$5c), 
    ($05,$be,$d5,$06), ($0c,$b3,$de,$08), ($17,$a4,$c3,$1a), ($1e,$a9,$c8,$14), 
    ($21,$8a,$f9,$3e), ($28,$87,$f2,$30), ($33,$90,$ef,$22), ($3a,$9d,$e4,$2c), 
    ($dd,$06,$3d,$96), ($d4,$0b,$36,$98), ($cf,$1c,$2b,$8a), ($c6,$11,$20,$84),
    ($f9,$32,$11,$ae), ($f0,$3f,$1a,$a0), ($eb,$28,$07,$b2), ($e2,$25,$0c,$bc), 
    ($95,$6e,$65,$e6), ($9c,$63,$6e,$e8), ($87,$74,$73,$fa), ($8e,$79,$78,$f4), 
    ($b1,$5a,$49,$de), ($b8,$57,$42,$d0), ($a3,$40,$5f,$c2), ($aa,$4d,$54,$cc),
    ($ec,$da,$f7,$41), ($e5,$d7,$fc,$4f), ($fe,$c0,$e1,$5d), ($f7,$cd,$ea,$53), 
    ($c8,$ee,$db,$79), ($c1,$e3,$d0,$77), ($da,$f4,$cd,$65), ($d3,$f9,$c6,$6b), 
    ($a4,$b2,$af,$31), ($ad,$bf,$a4,$3f), ($b6,$a8,$b9,$2d), ($bf,$a5,$b2,$23), 
    ($80,$86,$83,$09), ($89,$8b,$88,$07), ($92,$9c,$95,$15), ($9b,$91,$9e,$1b),
    ($7c,$0a,$47,$a1), ($75,$07,$4c,$af), ($6e,$10,$51,$bd), ($67,$1d,$5a,$b3), 
    ($58,$3e,$6b,$99), ($51,$33,$60,$97), ($4a,$24,$7d,$85), ($43,$29,$76,$8b), 
    ($34,$62,$1f,$d1), ($3d,$6f,$14,$df), ($26,$78,$09,$cd), ($2f,$75,$02,$c3), 
    ($10,$56,$33,$e9), ($19,$5b,$38,$e7), ($02,$4c,$25,$f5), ($0b,$41,$2e,$fb), 
    ($d7,$61,$8c,$9a), ($de,$6c,$87,$94), ($c5,$7b,$9a,$86), ($cc,$76,$91,$88),
    ($f3,$55,$a0,$a2), ($fa,$58,$ab,$ac), ($e1,$4f,$b6,$be), ($e8,$42,$bd,$b0),
    ($9f,$09,$d4,$ea), ($96,$04,$df,$e4), ($8d,$13,$c2,$f6), ($84,$1e,$c9,$f8),
    ($bb,$3d,$f8,$d2), ($b2,$30,$f3,$dc), ($a9,$27,$ee,$ce), ($a0,$2a,$e5,$c0),
    ($47,$b1,$3c,$7a), ($4e,$bc,$37,$74), ($55,$ab,$2a,$66), ($5c,$a6,$21,$68),
    ($63,$85,$10,$42), ($6a,$88,$1b,$4c), ($71,$9f,$06,$5e), ($78,$92,$0d,$50), 
    ($0f,$d9,$64,$0a), ($06,$d4,$6f,$04), ($1d,$c3,$72,$16), ($14,$ce,$79,$18),
    ($2b,$ed,$48,$32), ($22,$e0,$43,$3c), ($39,$f7,$5e,$2e), ($30,$fa,$55,$20),
    ($9a,$b7,$01,$ec), ($93,$ba,$0a,$e2), ($88,$ad,$17,$f0), ($81,$a0,$1c,$fe),
    ($be,$83,$2d,$d4), ($b7,$8e,$26,$da), ($ac,$99,$3b,$c8), ($a5,$94,$30,$c6),
    ($d2,$df,$59,$9c), ($db,$d2,$52,$92), ($c0,$c5,$4f,$80), ($c9,$c8,$44,$8e),
    ($f6,$eb,$75,$a4), ($ff,$e6,$7e,$aa), ($e4,$f1,$63,$b8), ($ed,$fc,$68,$b6),
    ($0a,$67,$b1,$0c), ($03,$6a,$ba,$02), ($18,$7d,$a7,$10), ($11,$70,$ac,$1e),
    ($2e,$53,$9d,$34), ($27,$5e,$96,$3a), ($3c,$49,$8b,$28), ($35,$44,$80,$26),
    ($42,$0f,$e9,$7c), ($4b,$02,$e2,$72), ($50,$15,$ff,$60), ($59,$18,$f4,$6e),
    ($66,$3b,$c5,$44), ($6f,$36,$ce,$4a), ($74,$21,$d3,$58), ($7d,$2c,$d8,$56),
    ($a1,$0c,$7a,$37), ($a8,$01,$71,$39), ($b3,$16,$6c,$2b), ($ba,$1b,$67,$25),
    ($85,$38,$56,$0f), ($8c,$35,$5d,$01), ($97,$22,$40,$13), ($9e,$2f,$4b,$1d),
    ($e9,$64,$22,$47), ($e0,$69,$29,$49), ($fb,$7e,$34,$5b), ($f2,$73,$3f,$55),
    ($cd,$50,$0e,$7f), ($c4,$5d,$05,$71), ($df,$4a,$18,$63), ($d6,$47,$13,$6d),
    ($31,$dc,$ca,$d7), ($38,$d1,$c1,$d9), ($23,$c6,$dc,$cb), ($2a,$cb,$d7,$c5),
    ($15,$e8,$e6,$ef), ($1c,$e5,$ed,$e1), ($07,$f2,$f0,$f3), ($0e,$ff,$fb,$fd),
    ($79,$b4,$92,$a7), ($70,$b9,$99,$a9), ($6b,$ae,$84,$bb), ($62,$a3,$8f,$b5),
    ($5d,$80,$be,$9f), ($54,$8d,$b5,$91), ($4f,$9a,$a8,$83), ($46,$97,$a3,$8d));

  rcon: array[0..29] of cardinal= (
    $01, $02, $04, $08, $10, $20, $40, $80, $1b, $36, $6c, $d8, $ab, $4d, $9a,
    $2f, $5e, $bc, $63, $c6, $97, $35, $6a, $d4, $b3, $7d, $fa, $ef, $c5, $91);

const
  BCRJ = 4;
  MAXROUNDSRJ= 14;


type
  PRijndael = ^TRijndael;
  TRijndael = object(TBlockCipher128)
  protected
    numrounds: longword;
    rk, drk: array[0..MAXROUNDSRJ,0..7] of DWord;

  public
    procedure InitKey(const Key; Size: longword); virtual;
    procedure Burn;  virtual;
    procedure EncryptECB(const InData; var OutData);  virtual;
    procedure DecryptECB(const InData; var OutData);  virtual;

    destructor Destroy; virtual;
  end;

// Serpent cipher implementation

type
  PSerpent = ^TSerpent;
  TSerpent = object(TBlockCipher128)
  protected
    l_key: array[0..131] of dword;


  public
    procedure InitKey(const Key; Size: longword); virtual;
    procedure Burn;  virtual;
    procedure EncryptECB(const InData; var OutData);  virtual;
    procedure DecryptECB(const InData; var OutData);  virtual;

    destructor Destroy; virtual;
  end;

// TwoFish cipher implementation

const
  p8x8: array[0..1,0..255] of byte= ((
    $a9, $67, $b3, $e8, $04, $fd, $a3, $76,
    $9a, $92, $80, $78, $e4, $dd, $d1, $38,
    $0d, $c6, $35, $98, $18, $f7, $ec, $6c,
    $43, $75, $37, $26, $fa, $13, $94, $48,
    $f2, $d0, $8b, $30, $84, $54, $df, $23,
    $19, $5b, $3d, $59, $f3, $ae, $a2, $82,
    $63, $01, $83, $2e, $d9, $51, $9b, $7c,
    $a6, $eb, $a5, $be, $16, $0c, $e3, $61,
    $c0, $8c, $3a, $f5, $73, $2c, $25, $0b,
    $bb, $4e, $89, $6b, $53, $6a, $b4, $f1,
    $e1, $e6, $bd, $45, $e2, $f4, $b6, $66,
    $cc, $95, $03, $56, $d4, $1c, $1e, $d7,
    $fb, $c3, $8e, $b5, $e9, $cf, $bf, $ba,
    $ea, $77, $39, $af, $33, $c9, $62, $71,
    $81, $79, $09, $ad, $24, $cd, $f9, $d8,
    $e5, $c5, $b9, $4d, $44, $08, $86, $e7,
    $a1, $1d, $aa, $ed, $06, $70, $b2, $d2,
    $41, $7b, $a0, $11, $31, $c2, $27, $90, 
    $20, $f6, $60, $ff, $96, $5c, $b1, $ab, 
    $9e, $9c, $52, $1b, $5f, $93, $0a, $ef,
    $91, $85, $49, $ee, $2d, $4f, $8f, $3b,
    $47, $87, $6d, $46, $d6, $3e, $69, $64,
    $2a, $ce, $cb, $2f, $fc, $97, $05, $7a,
    $ac, $7f, $d5, $1a, $4b, $0e, $a7, $5a,
    $28, $14, $3f, $29, $88, $3c, $4c, $02,
    $b8, $da, $b0, $17, $55, $1f, $8a, $7d,
    $57, $c7, $8d, $74, $b7, $c4, $9f, $72,
    $7e, $15, $22, $12, $58, $07, $99, $34,
    $6e, $50, $de, $68, $65, $bc, $db, $f8,
    $c8, $a8, $2b, $40, $dc, $fe, $32, $a4,
    $ca, $10, $21, $f0, $d3, $5d, $0f, $00,
    $6f, $9d, $36, $42, $4a, $5e, $c1, $e0),(
    $75, $f3, $c6, $f4, $db, $7b, $fb, $c8,
    $4a, $d3, $e6, $6b, $45, $7d, $e8, $4b,
    $d6, $32, $d8, $fd, $37, $71, $f1, $e1,
    $30, $0f, $f8, $1b, $87, $fa, $06, $3f,
    $5e, $ba, $ae, $5b, $8a, $00, $bc, $9d,
    $6d, $c1, $b1, $0e, $80, $5d, $d2, $d5,
    $a0, $84, $07, $14, $b5, $90, $2c, $a3,
    $b2, $73, $4c, $54, $92, $74, $36, $51,
    $38, $b0, $bd, $5a, $fc, $60, $62, $96,
    $6c, $42, $f7, $10, $7c, $28, $27, $8c,
    $13, $95, $9c, $c7, $24, $46, $3b, $70,
    $ca, $e3, $85, $cb, $11, $d0, $93, $b8,
    $a6, $83, $20, $ff, $9f, $77, $c3, $cc,
    $03, $6f, $08, $bf, $40, $e7, $2b, $e2,
    $79, $0c, $aa, $82, $41, $3a, $ea, $b9,
    $e4, $9a, $a4, $97, $7e, $da, $7a, $17,
    $66, $94, $a1, $1d, $3d, $f0, $de, $b3,
    $0b, $72, $a7, $1c, $ef, $d1, $53, $3e,
    $8f, $33, $26, $5f, $ec, $76, $2a, $49,
    $81, $88, $ee, $21, $c4, $1a, $eb, $d9,
    $c5, $39, $99, $cd, $ad, $31, $8b, $01,
    $18, $23, $dd, $1f, $4e, $2d, $f9, $48,
    $4f, $f2, $65, $8e, $78, $5c, $58, $19,
    $8d, $e5, $98, $57, $67, $7f, $05, $64,
    $af, $63, $b6, $fe, $f5, $b7, $3c, $a5,
    $ce, $e9, $68, $44, $e0, $4d, $43, $69,
    $29, $2e, $ac, $15, $59, $a8, $0a, $9e,
    $6e, $47, $df, $34, $35, $6a, $cf, $dc,
    $22, $c9, $c0, $9b, $89, $d4, $ed, $ab,
    $12, $a2, $0d, $52, $bb, $02, $2f, $a9,
    $d7, $61, $1e, $b4, $50, $04, $f6, $c2,
    $16, $25, $86, $56, $55, $09, $be, $91));

const
  INPUTWHITEN= 0;
  OUTPUTWHITEN= 4;
  NUMROUNDSTF= 16;
  ROUNDSUBKEYS= (OUTPUTWHITEN + 4);
  TOTALSUBKEYS= (ROUNDSUBKEYS + NUMROUNDSTF * 2);
  RS_GF_FDBK= $14d;
  MDS_GF_FDBK= $169;
  SK_STEP= $02020202;
  SK_BUMP= $01010101;
  SK_ROTL= 9;

type
  PTwoFish = ^TTwoFish;
  TTwoFish = object(TBlockCipher128)
  protected
    SubKeys: array[0..TOTALSUBKEYS-1] of DWord;
    sbox: array[0..3,0..255] of DWord;

  public
    procedure InitKey(const Key; Size: longword); virtual;
    procedure Burn;  virtual;
    procedure EncryptECB(const InData; var OutData);  virtual;
    procedure DecryptECB(const InData; var OutData);  virtual;

    destructor Destroy; virtual;
  end;

type
  PRC4 = ^TRC4;

  TRC4 = object (TObj)
  protected
    KeyData, KeyOrg: array[0..255] of byte;
  public
    procedure InitKey(const Key; Size: longword; InitVector: pointer);
    procedure Reset;
    procedure Burn;
    procedure Encrypt(const InData; var OutData; Size: longword);
    procedure Decrypt(const InData; var OutData; Size: longword);
    destructor Destroy; virtual;
  end;


  TKOLRijndael = PRijndael;
  TKOLMars = PMars;
  TKOLICE = PICE;
  TKOLICE2 = PICE2;
  TKOLThinICE = PThinICE;
  TKOLDES = PDES;
  TKOL3DES = P3DES;
  TKOLTEA = PTEA;
  TKOLRC2 = PRC2;
  TKOLRC4 = PRC4;
  TKOLRC5 = PRC5;
  TKOLRC6 = PRC6;
  TKOLMisty1 = PMisty1;
  TKOLIDEA = PIDEA;
  TKOLGOST = PGOST;
  TKOLCast128 = PCast128;
  TKOLCast256 = PCast256;
  TKOLBlowfish = PBlowfish;
  TKOLTwofish = PTwofish;
  TKOLSerpent = PSerpent;




//  TKOLBlockCipher64 = PBlockCipher64;
//  TKOLBlockCipher128 = PBlockCipher128;
function NewSerpent: PSerpent;
function NewRijndael: PRijndael;
function NewICE: PICE;
function NewICE2: PICE2;
function NewThinICE: PThinICE;
function New3DES: P3DES;
function NewDES: PDES;
function NewTEA: PTEA;
function NewRC2: PRC2;
function NewRC4: PRC4;
function NewRC5: PRC5;
function NewRC6: PRC6;
function NewMisty1: PMisty1;
function NewIDEA: PIDEA;
function NewGOST: PGOST;
function NewCast256: PCast256;
function NewCast128: PCast128;
function NewBlowfish: PBlowfish;
function NewTwofish: PTwofish;
function NewMars: PMars;

//function NewBlockCipher64: PBlockCipher64;
//function NewBlockCipher128: PBlockCipher128;


implementation

// uses CommCtrl, ShellApi;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TBlockCipher64.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
//function NewBlockCipher64;
//begin
//New(Result, Create);

// code
//end;
////////////////////////////////////////////////////////////////////////////////

destructor TBlockCipher128.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
//function NewBlockCipher128;
//begin
//New(Result, Create);

// code
//end;
////////////////////////////////////////////////////////////////////////////////
procedure XorBlock(var InData1, InData2; Size: longword);
var
  i: longword;
begin
  for i:= 1 to Size do
    Pbyte(longword(@InData1)+i-1)^:= Pbyte(longword(@InData1)+i-1)^ xor Pbyte(longword(@InData2)+i-1)^;
end;


procedure TBlockCipher64.EncryptECB(const Indata; var Outdata);
begin
end;

procedure TBlockCipher64.DecryptECB(const Indata; var Outdata);
begin
end;

procedure TBlockCipher64.InitKey(const Key; Size: longword);
begin
end;


procedure TBlockCipher128.EncryptECB(const Indata; var Outdata);
begin
end;

procedure TBlockCipher128.DecryptECB(const Indata; var Outdata);
begin
end;

procedure TBlockCipher128.InitKey(const Key; Size: longword);
begin
end;

procedure TBlockCipher64.IncCounter;
var
  i: integer;
begin
  Inc(CV[7]);
  i:= 7;
  while (i> 0) and (CV[i] = 0) do
  begin
    Inc(CV[i-1]);
    Dec(i);
  end;
end;

procedure TBlockCipher64.InitBlockCipher64(const Key; Size: longword; InitVector: pointer);
begin
  {inherited}
  //Init(Key,Size,InitVector);
  InitKey(Key,Size);
  if InitVector= nil then
  begin
    FillChar(IV,8,0);
    EncryptECB(IV,IV);
    Reset;
  end
  else
  begin
    Move(InitVector^,IV,8);
    Reset;
  end;
end;

procedure TBlockCipher64.SetIV(const Value);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Move(Value,IV,8);
  Reset;
end;

procedure TBlockCipher64.GetIV(var Value);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Move(CV,Value,8);
end;

procedure TBlockCipher64.Reset;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized')
//  else
    Move(IV,CV,8);
end;

procedure TBlockCipher64.Burn;
begin
  FillChar(IV,8,$FF);
  FillChar(CV,8,$FF);
//  inherited Burn;
end;

procedure TBlockCipher64.EncryptCBC(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    Move(p1^,p2^,8);
    XorBlock(p2^,CV,8);
    EncryptECB(p2^,p2^);
    Move(p2^,CV,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,CV,Size mod 8);
  end;
end;

procedure TBlockCipher64.DecryptCBC(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
  Temp: array[0..7] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    Move(p1^,p2^,8);
    Move(p1^,Temp,8);
    DecryptECB(p2^,p2^);
    XorBlock(p2^,CV,8);
    Move(Temp,CV,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,CV,Size mod 8);
  end;
end;

procedure TBlockCipher64.EncryptCFB8bit(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
  Temp: array[0..7] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to Size do
  begin
    EncryptECB(CV,Temp);
    p2^:= p1^ xor Temp[0];
    Move(CV[1],CV[0],8-1);
    CV[7]:= p2^;
    Inc(p1);
    Inc(p2);
  end;
end;

procedure TBlockCipher64.DecryptCFB8bit(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
  TempByte: byte;
  Temp: array[0..7] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to Size do
  begin
    TempByte:= p1^;
    EncryptECB(CV,Temp);
    p2^:= p1^ xor Temp[0];
    Move(CV[1],CV[0],8-1);
    CV[7]:= TempByte;
    Inc(p1);
    Inc(p2);
  end;
end;

procedure TBlockCipher64.EncryptCFBblock(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,8);
    XorBlock(p2^,CV,8);
    Move(p2^,CV,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,CV,Size mod 8);
  end;
end;

procedure TBlockCipher64.DecryptCFBblock(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
  Temp: array[0..7] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    Move(p1^,Temp,8);
    EncryptECB(CV,CV);
    Move(p1^,p2^,8);
    XorBlock(p2^,CV,8);
    Move(Temp,CV,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,CV,Size mod 8);
  end;
end;

procedure TBlockCipher64.EncryptOFB(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,8);
    XorBlock(p2^,CV,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,CV,Size mod 8);
  end;
end;

procedure TBlockCipher64.DecryptOFB(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,8);
    XorBlock(p2^,CV,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,CV,Size mod 8);
  end;
end;

procedure TBlockCipher64.EncryptCTR(const Indata; var Outdata; Size: longword);
var
  temp: array[0..7] of byte;
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,8);
    XorBlock(p2^,temp,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,temp,Size mod 8);
  end;
end;

procedure TBlockCipher64.DecryptCTR(const Indata; var Outdata; Size: longword);
var
  temp: array[0..7] of byte;
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 8) do
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,8);
    XorBlock(p2^,temp,8);
    p1:= pointer(longword(p1) + 8);
    p2:= pointer(longword(p2) + 8);
  end;
  if (Size mod 8)<> 0 then
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,Size mod 8);
    XorBlock(p2^,temp,Size mod 8);
  end;
end;

{** TBlockCipher128 ********************************************************}

procedure TBlockCipher128.IncCounter;
var
  i: integer;
begin
  Inc(CV[15]);
  i:= 15;
  while (i> 0) and (CV[i] = 0) do
  begin
    Inc(CV[i-1]);
    Dec(i);
  end;
end;

procedure TBlockCipher128.InitBlockCipher128(const Key; Size: longword; InitVector: pointer);
begin
//  inherited Init(Key,Size,InitVector);
  InitKey(Key,Size);
  if InitVector= nil then
  begin
    FillChar(IV,16,0);
    EncryptECB(IV,IV);
    Reset;
  end
  else
  begin
    Move(InitVector^,IV,16);
    Reset;
  end;
end;

procedure TBlockCipher128.SetIV(const Value);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Move(Value,IV,16);
  Reset;
end;

procedure TBlockCipher128.GetIV(var Value);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Move(CV,Value,16);
end;

procedure TBlockCipher128.Reset;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized')
//  else
    Move(IV,CV,16);
end;

procedure TBlockCipher128.Burn;
begin
  FillChar(IV,16,$FF);
  FillChar(CV,16,$FF);
//  inherited Burn;
end;

procedure TBlockCipher128.EncryptCBC(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    Move(p1^,p2^,16);
    XorBlock(p2^,CV,16);
    EncryptECB(p2^,p2^);
    Move(p2^,CV,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,CV,Size mod 16);
  end;
end;

procedure TBlockCipher128.DecryptCBC(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
  Temp: array[0..15] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    Move(p1^,p2^,16);
    Move(p1^,Temp,16);
    DecryptECB(p2^,p2^);
    XorBlock(p2^,CV,16);
    Move(Temp,CV,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,CV,Size mod 16);
  end;
end;

procedure TBlockCipher128.EncryptCFB8bit(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
  Temp: array[0..15] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to Size do
  begin
    EncryptECB(CV,Temp);
    p2^:= p1^ xor Temp[0];
    Move(CV[1],CV[0],15);
    CV[15]:= p2^;
    Inc(p1);
    Inc(p2);
  end;
end;

procedure TBlockCipher128.DecryptCFB8bit(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
  TempByte: byte;
  Temp: array[0..15] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to Size do
  begin
    TempByte:= p1^;
    EncryptECB(CV,Temp);
    p2^:= p1^ xor Temp[0];
    Move(CV[1],CV[0],15);
    CV[15]:= TempByte;
    Inc(p1);
    Inc(p2);
  end;
end;

procedure TBlockCipher128.EncryptCFBblock(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,16);
    XorBlock(p2^,CV,16);
    Move(p2^,CV,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,CV,Size mod 16);
  end;
end;

procedure TBlockCipher128.DecryptCFBblock(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: Pbyte;
  Temp: array[0..15] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    Move(p1^,Temp,16);
    EncryptECB(CV,CV);
    Move(p1^,p2^,16);
    XorBlock(p2^,CV,16);
    Move(Temp,CV,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,CV,Size mod 16);
  end;
end;

procedure TBlockCipher128.EncryptOFB(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,16);
    XorBlock(p2^,CV,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,CV,Size mod 16);
  end;
end;

procedure TBlockCipher128.DecryptOFB(const Indata; var Outdata; Size: longword);
var
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,16);
    XorBlock(p2^,CV,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,CV);
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,CV,Size mod 16);
  end;
end;

procedure TBlockCipher128.EncryptCTR(const Indata; var Outdata; Size: longword);
var
  temp: array[0..15] of byte;
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,16);
    XorBlock(p2^,temp,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,temp,Size mod 16);
  end;
end;

procedure TBlockCipher128.DecryptCTR(const Indata; var Outdata; Size: longword);
var
  temp: array[0..15] of byte;
  i: longword;
  p1, p2: pointer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  p1:= @Indata;
  p2:= @Outdata;
  for i:= 1 to (Size div 16) do
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,16);
    XorBlock(p2^,temp,16);
    p1:= pointer(longword(p1) + 16);
    p2:= pointer(longword(p2) + 16);
  end;
  if (Size mod 16)<> 0 then
  begin
    EncryptECB(CV,temp);
    IncCounter;
    Move(p1^,p2^,Size mod 16);
    XorBlock(p2^,temp,Size mod 16);
  end;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TBlowfish.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewBlowfish;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////

{$R-}{$Q-}
//{$I DCPblowfish.inc}

procedure TBlowfish.InitKey(const Key; Size: longword);
var
  i, k: longword;
  A: DWord;
  KeyB: PByteArray;
  Block: array[0..7] of byte;
begin
burn;
  Size:= Size div 8;
  KeyB:= @Key;
  Move(SBoxOrg,SBox,Sizeof(SBox));
  Move(PBoxOrg,PBox,Sizeof(PBox));
  k:= 0;
  for i:= 0 to 17 do
  begin
    A:= dword(KeyB^[(k+3) mod Size]);
    A:= A + (dword(KeyB^[(k+2) mod Size]) shl 8);
    A:= A + (dword(KeyB^[(k+1) mod Size]) shl 16);
    A:= A + (dword(KeyB^[k]) shl 24);
    PBox[i]:= PBox[i] xor A;
    k:= (k+4) mod Size;
  end;
  FillChar(Block,Sizeof(Block),0);
  for i:= 0 to 8 do
  begin
    EncryptECB(Block,Block);
    PBox[i*2]:= dword(Block[3]) + (dword(Block[2]) shl 8) + (dword(Block[1]) shl 16) + (dword(Block[0]) shl 24);
    PBox[i*2+1]:= dword(Block[7]) + (dword(Block[6]) shl 8) + (dword(Block[5]) shl 16) + (dword(Block[4]) shl 24);
  end;
  for k:= 0 to 3 do
  begin
    for i:= 0 to 127 do
    begin
      EncryptECB(Block,Block);
      SBox[k,i*2]:= dword(Block[3]) + (dword(Block[2]) shl 8) + (dword(Block[1]) shl 16) + (dword(Block[0]) shl 24);
      SBox[k,i*2+1]:= dword(Block[7]) + (dword(Block[6]) shl 8) + (dword(Block[5]) shl 16) + (dword(Block[4]) shl 24);
    end;
  end;
end;

procedure TBlowfish.Burn;
begin
  FillChar(SBox,Sizeof(SBox),$FF);
  FillChar(PBox,Sizeof(PBox),$FF);
  inherited Burn;
end;

procedure TBlowfish.EncryptECB(const InData; var OutData);
var
  xL, xR: DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  xL:= Pdword(@InData)^;
  xR:= Pdword(longword(@InData)+4)^;
  xL:= ((xL and $FF) shl 24) or ((xL and $FF00) shl 8) or ((xL and $FF0000) shr 8) or ((xL and $FF000000) shr 24);
  xR:= ((xR and $FF) shl 24) or ((xR and $FF00) shl 8) or ((xR and $FF0000) shr 8) or ((xR and $FF000000) shr 24);
  xL:= xL xor PBox[0];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[1];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[2];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[3];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[4];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[5];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[6];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[7];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[8];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[9];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[10];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[11];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[12];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[13];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[14];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[15];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[16];
  xR:= xR xor PBox[17];
  xL:= ((xL and $FF) shl 24) or ((xL and $FF00) shl 8) or ((xL and $FF0000) shr 8) or ((xL and $FF000000) shr 24);
  xR:= ((xR and $FF) shl 24) or ((xR and $FF00) shl 8) or ((xR and $FF0000) shr 8) or ((xR and $FF000000) shr 24);
  Pdword(@OutData)^:= xR;
  Pdword(longword(@OutData)+4)^:= xL;
end;

procedure TBlowfish.DecryptECB(const InData; var OutData);
var
  xL, xR: DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  xL:= Pdword(@InData)^;
  xR:= Pdword(longword(@InData)+4)^;
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  xL:= xL xor PBox[17];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[16];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[15];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[14];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[13];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[12];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[11];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[10];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[9];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[8];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[7];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[6];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[5];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[4];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[3];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[2];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[1];
  xR:= xR xor PBox[0];
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  Pdword(@OutData)^:= xR;
  Pdword(longword(@OutData)+4)^:= xL;
end;

{$R-}{$Q-}

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TCast128.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewCast128;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////


function LRot32(a, n: dword): dword;
begin
  Result:= (a shl n) or (a shr (32-n));
end;


procedure TCast128.InitKey(const Key; Size: longword);
var
  x, t, z: array[0..3] of DWord;
  i: longword;
begin
burn;
  Size:= Size div 8;
  if Size<= 10 then
    Rounds:= 12
  else
    Rounds:= 16;
  FillChar(x,Sizeof(x),0);
  Move(Key,x,Size);
  x[0]:= (x[0] shr 24) or ((x[0] shr 8) and $FF00) or ((x[0] shl 8) and $FF0000) or (x[0] shl 24);
  x[1]:= (x[1] shr 24) or ((x[1] shr 8) and $FF00) or ((x[1] shl 8) and $FF0000) or (x[1] shl 24);
  x[2]:= (x[2] shr 24) or ((x[2] shr 8) and $FF00) or ((x[2] shl 8) and $FF0000) or (x[2] shl 24);
  x[3]:= (x[3] shr 24) or ((x[3] shr 8) and $FF00) or ((x[3] shl 8) and $FF0000) or (x[3] shl 24);
  i:= 0;
  while i< 32 do
  begin
    case (i and 4) of
      0:
        begin
          z[0]:= x[0] xor cast_sbox5[(x[3] shr 16) and $FF] xor
           cast_sbox6[x[3] and $FF] xor cast_sbox7[x[3] shr 24] xor
           cast_sbox8[(x[3] shr 8) and $FF] xor cast_sbox7[x[2] shr 24];
          t[0]:= z[0];
          z[1]:= x[2] xor cast_sbox5[z[0] shr 24] xor
           cast_sbox6[(z[0] shr 8) and $FF] xor cast_sbox7[(z[0] shr 16) and $FF] xor
           cast_sbox8[z[0] and $FF] xor cast_sbox8[(x[2] shr 8) and $FF];
          t[1]:= z[1];
          z[2]:= x[3] xor cast_sbox5[z[1] and $FF] xor
           cast_sbox6[(z[1] shr 8) and $FF] xor cast_sbox7[(z[1] shr 16) and $FF] xor
           cast_sbox8[z[1] shr 24] xor cast_sbox5[(x[2] shr 16) and $FF];
          t[2]:= z[2];
          z[3]:= x[1] xor cast_sbox5[(z[2] shr 8) and $FF] xor
           cast_sbox6[(z[2] shr 16) and $FF] xor cast_sbox7[z[2] and $FF] xor
           cast_sbox8[z[2] shr 24] xor cast_sbox6[x[2] and $FF];
          t[3]:= z[3];
        end;
      4:
        begin
          x[0]:= z[2] xor cast_sbox5[(z[1] shr 16) and $FF] xor
           cast_sbox6[z[1] and $FF] xor cast_sbox7[z[1] shr 24] xor
           cast_sbox8[(z[1] shr 8) and $FF] xor cast_sbox7[z[0] shr 24];
          t[0]:= x[0];
          x[1]:= z[0] xor cast_sbox5[x[0] shr 24] xor
           cast_sbox6[(x[0] shr 8) and $FF] xor cast_sbox7[(x[0] shr 16) and $FF] xor
           cast_sbox8[x[0] and $FF] xor cast_sbox8[(z[0] shr 8) and $FF];
          t[1]:= x[1];
          x[2]:= z[1] xor cast_sbox5[x[1] and $FF] xor
           cast_sbox6[(x[1] shr 8) and $FF] xor cast_sbox7[(x[1] shr 16) and $FF] xor
           cast_sbox8[x[1] shr 24] xor cast_sbox5[(z[0] shr 16) and $FF];
          t[2]:= x[2];
          x[3]:= z[3] xor cast_sbox5[(x[2] shr 8) and $FF] xor
           cast_sbox6[(x[2] shr 16) and $FF] xor cast_sbox7[x[2] and $FF] xor
           cast_sbox8[x[2] shr 24] xor cast_sbox6[z[0] and $FF];
          t[3]:= x[3];
        end;
    end;
    case (i and 12) of
      0,12:
        begin
          KeyData[i+0]:= cast_sbox5[t[2] shr 24] xor cast_sbox6[(t[2] shr 16) and $FF] xor
           cast_sbox7[t[1] and $FF] xor cast_sbox8[(t[1] shr 8) and $FF];
          KeyData[i+1]:= cast_sbox5[(t[2] shr 8) and $FF] xor cast_sbox6[t[2] and $FF] xor
           cast_sbox7[(t[1] shr 16) and $FF] xor cast_sbox8[t[1] shr 24];
          KeyData[i+2]:= cast_sbox5[t[3] shr 24] xor cast_sbox6[(t[3] shr 16) and $FF] xor
           cast_sbox7[t[0] and $FF] xor cast_sbox8[(t[0] shr 8) and $FF];
          KeyData[i+3]:= cast_sbox5[(t[3] shr 8) and $FF] xor cast_sbox6[t[3] and $FF] xor
           cast_sbox7[(t[0] shr 16) and $FF] xor cast_sbox8[t[0] shr 24];
        end;
      4,8:
        begin
          KeyData[i+0]:= cast_sbox5[t[0] and $FF] xor cast_sbox6[(t[0] shr 8) and $FF] xor
           cast_sbox7[t[3] shr 24] xor cast_sbox8[(t[3] shr 16) and $FF];
          KeyData[i+1]:= cast_sbox5[(t[0] shr 16) and $FF] xor cast_sbox6[t[0] shr 24] xor
           cast_sbox7[(t[3] shr 8) and $FF] xor cast_sbox8[t[3] and $FF];
          KeyData[i+2]:= cast_sbox5[t[1] and $FF] xor cast_sbox6[(t[1] shr 8) and $FF] xor
           cast_sbox7[t[2] shr 24] xor cast_sbox8[(t[2] shr 16) and $FF];
          KeyData[i+3]:= cast_sbox5[(t[1] shr 16) and $FF] xor cast_sbox6[t[1] shr 24] xor
           cast_sbox7[(t[2] shr 8) and $FF] xor cast_sbox8[t[2] and $FF];
        end;
    end;
    case (i and 12) of
      0:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[(z[0] shr 8) and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[(z[1] shr 8) and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[(z[2] shr 16) and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[z[3] shr 24];
        end;
      4:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[x[2] shr 24];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[(x[3] shr 16) and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[x[0] and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[x[1] and $FF];
        end;
      8:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[(z[2] shr 16) and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[z[3] shr 24];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[(z[0] shr 8) and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[(z[1] shr 8) and $FF];
        end;
      12:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[x[0] and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[x[1] and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[x[2] shr 24];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[(x[3] shr 16) and $FF];
        end;
    end;
    if (i >= 16) then
    begin
      KeyData[i+0]:= KeyData[i+0] and 31;
      KeyData[i+1]:= KeyData[i+1] and 31;
      KeyData[i+2]:= KeyData[i+2] and 31;
      KeyData[i+3]:= KeyData[i+3] and 31;
    end;
    Inc(i,4);
  end;
end;

procedure TCast128.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),$FF);
  Rounds:= 0;
  inherited Burn;
end;

procedure TCast128.EncryptECB(const InData; var OutData);
var
  t, l, r: DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  l:= Pdword(@InData)^;
  r:= Pdword(longword(@InData)+4)^;
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  t:= LRot32(KeyData[0]+r, KeyData[0+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[1] xor l, KeyData[1+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[2]-r, KeyData[2+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[3]+l, KeyData[3+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[4] xor r, KeyData[4+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[5]-l, KeyData[5+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[6]+r, KeyData[6+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[7] xor l, KeyData[7+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[8]-r, KeyData[8+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[9]+l, KeyData[9+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[10] xor r, KeyData[10+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[11]-l, KeyData[11+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  if Rounds> 12 then
  begin
    t:= LRot32(KeyData[12]+r, KeyData[12+16]);
    l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[13] xor l, KeyData[13+16]);
    r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
      cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[14]-r, KeyData[14+16]);
    l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
      cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[15]+l, KeyData[15+16]);
    r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  end;
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  Pdword(@OutData)^:= r;
  Pdword(longword(@OutData)+4)^:= l;
end;

procedure TCast128.DecryptECB(const InData; var OutData);
var
  t, l, r: DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  r:= Pdword(@InData)^;
  l:= Pdword(longword(@InData)+4)^;
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  if Rounds> 12 then
  begin
    t:= LRot32(KeyData[15]+l, KeyData[15+16]);
    r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[14]-r, KeyData[14+16]);
    l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
      cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[13] xor l, KeyData[13+16]);
    r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
      cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[12]+r, KeyData[12+16]);
    l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  end;
  t:= LRot32(KeyData[11]-l, KeyData[11+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[10] xor r, KeyData[10+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[9]+l, KeyData[9+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[8]-r, KeyData[8+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[7] xor l, KeyData[7+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[6]+r, KeyData[6+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[5]-l, KeyData[5+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[4] xor r, KeyData[4+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[3]+l, KeyData[3+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[2]-r, KeyData[2+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[1] xor l, KeyData[1+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[0]+r, KeyData[0+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  Pdword(@OutData)^:= l;
  Pdword(longword(@OutData)+4)^:= r;
end;

destructor TGOST.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewGOST;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////
{$R-}{$Q-}
//{$I DCPgost.inc}


procedure TGOST.InitKey(const Key; Size: longword);
var
  i: longword;
  userkey: array[0..31] of byte;
begin
burn;

  Size:= Size div 8;

  FillChar(userkey,Sizeof(userkey),0);
  Move(Key,userkey,Size);
  for i:= 0 to 7 do
    KeyData[i]:= (dword(UserKey[4*i+3]) shl 24) or (dword(UserKey[4*i+2]) shl 16) or
      (dword(UserKey[4*i+1]) shl 8) or (dword(UserKey[4*i+0]));
end;

procedure TGOST.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TGOST.EncryptECB(const InData; var OutData);
var
  n1, n2: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  n1:= PDword(@InData)^;
  n2:= PDword(dword(@InData)+4)^;
  for i:= 0 to 2 do
  begin
    n2:= n2 xor (sTable[3,(n1+KeyData[0]) shr 24] xor sTable[2,((n1+KeyData[0]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[0]) shr 8) and $FF] xor sTable[0,(n1+KeyData[0]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[1]) shr 24] xor sTable[2,((n2+KeyData[1]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[1]) shr 8) and $FF] xor sTable[0,(n2+KeyData[1]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[2]) shr 24] xor sTable[2,((n1+KeyData[2]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[2]) shr 8) and $FF] xor sTable[0,(n1+KeyData[2]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[3]) shr 24] xor sTable[2,((n2+KeyData[3]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[3]) shr 8) and $FF] xor sTable[0,(n2+KeyData[3]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[4]) shr 24] xor sTable[2,((n1+KeyData[4]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[4]) shr 8) and $FF] xor sTable[0,(n1+KeyData[4]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[5]) shr 24] xor sTable[2,((n2+KeyData[5]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[5]) shr 8) and $FF] xor sTable[0,(n2+KeyData[5]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[6]) shr 24] xor sTable[2,((n1+KeyData[6]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[6]) shr 8) and $FF] xor sTable[0,(n1+KeyData[6]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[7]) shr 24] xor sTable[2,((n2+KeyData[7]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[7]) shr 8) and $FF] xor sTable[0,(n2+KeyData[7]) and $FF]);
  end;
  n2:= n2 xor (sTable[3,(n1+KeyData[7]) shr 24] xor sTable[2,((n1+KeyData[7]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[7]) shr 8) and $FF] xor sTable[0,(n1+KeyData[7]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[6]) shr 24] xor sTable[2,((n2+KeyData[6]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[6]) shr 8) and $FF] xor sTable[0,(n2+KeyData[6]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[5]) shr 24] xor sTable[2,((n1+KeyData[5]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[5]) shr 8) and $FF] xor sTable[0,(n1+KeyData[5]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[4]) shr 24] xor sTable[2,((n2+KeyData[4]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[4]) shr 8) and $FF] xor sTable[0,(n2+KeyData[4]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[3]) shr 24] xor sTable[2,((n1+KeyData[3]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[3]) shr 8) and $FF] xor sTable[0,(n1+KeyData[3]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[2]) shr 24] xor sTable[2,((n2+KeyData[2]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[2]) shr 8) and $FF] xor sTable[0,(n2+KeyData[2]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[1]) shr 24] xor sTable[2,((n1+KeyData[1]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[1]) shr 8) and $FF] xor sTable[0,(n1+KeyData[1]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[0]) shr 24] xor sTable[2,((n2+KeyData[0]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[0]) shr 8) and $FF] xor sTable[0,(n2+KeyData[0]) and $FF]);
  PDword(@OutData)^:= n2;
  PDword(dword(@OutData)+4)^:= n1;
end;

procedure TGOST.DecryptECB(const InData; var OutData);
var
  n1, n2: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  n1:= PDword(@InData)^;
  n2:= PDword(dword(@InData)+4)^;
  n2:= n2 xor (sTable[3,(n1+KeyData[0]) shr 24] xor sTable[2,((n1+KeyData[0]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[0]) shr 8) and $FF] xor sTable[0,(n1+KeyData[0]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[1]) shr 24] xor sTable[2,((n2+KeyData[1]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[1]) shr 8) and $FF] xor sTable[0,(n2+KeyData[1]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[2]) shr 24] xor sTable[2,((n1+KeyData[2]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[2]) shr 8) and $FF] xor sTable[0,(n1+KeyData[2]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[3]) shr 24] xor sTable[2,((n2+KeyData[3]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[3]) shr 8) and $FF] xor sTable[0,(n2+KeyData[3]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[4]) shr 24] xor sTable[2,((n1+KeyData[4]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[4]) shr 8) and $FF] xor sTable[0,(n1+KeyData[4]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[5]) shr 24] xor sTable[2,((n2+KeyData[5]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[5]) shr 8) and $FF] xor sTable[0,(n2+KeyData[5]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[6]) shr 24] xor sTable[2,((n1+KeyData[6]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[6]) shr 8) and $FF] xor sTable[0,(n1+KeyData[6]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[7]) shr 24] xor sTable[2,((n2+KeyData[7]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[7]) shr 8) and $FF] xor sTable[0,(n2+KeyData[7]) and $FF]);
  for i:= 0 to 2 do
  begin
    n2:= n2 xor (sTable[3,(n1+KeyData[7]) shr 24] xor sTable[2,((n1+KeyData[7]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[7]) shr 8) and $FF] xor sTable[0,(n1+KeyData[7]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[6]) shr 24] xor sTable[2,((n2+KeyData[6]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[6]) shr 8) and $FF] xor sTable[0,(n2+KeyData[6]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[5]) shr 24] xor sTable[2,((n1+KeyData[5]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[5]) shr 8) and $FF] xor sTable[0,(n1+KeyData[5]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[4]) shr 24] xor sTable[2,((n2+KeyData[4]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[4]) shr 8) and $FF] xor sTable[0,(n2+KeyData[4]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[3]) shr 24] xor sTable[2,((n1+KeyData[3]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[3]) shr 8) and $FF] xor sTable[0,(n1+KeyData[3]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[2]) shr 24] xor sTable[2,((n2+KeyData[2]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[2]) shr 8) and $FF] xor sTable[0,(n2+KeyData[2]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[1]) shr 24] xor sTable[2,((n1+KeyData[1]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[1]) shr 8) and $FF] xor sTable[0,(n1+KeyData[1]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[0]) shr 24] xor sTable[2,((n2+KeyData[0]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[0]) shr 8) and $FF] xor sTable[0,(n2+KeyData[0]) and $FF]);
  end;
  PDword(@OutData)^:= n2;
  PDword(dword(@OutData)+4)^:= n1;
end;

destructor TIDEA.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewIDEA;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////

{$R-}{$Q-}


function MulInv(x: word): word;
var
  t0, t1, q, y: word;
begin
  if x<= 1 then
  begin
    Result:= x;
    Exit;
  end;
  t1:= DWord($10001) div x;
  y:= DWord($10001) mod x;
  if y= 1 then
  begin
    Result:= (1 - t1) and $FFFF;
    Exit;
  end;
  t0:= 1;
  repeat
    q:= x div y;
    x:= x mod y;
    t0:= t0 + (q*t1);
    if x= 1 then
    begin
      Result:= t0;
      Exit;
    end;
    q:= y div x;
    y:= y mod x;
    t1:= t1 + (q*t0);
  until y= 1;
  Result:= (1-t1) and $FFFF;
end;

procedure TIDEA.InitKey(const Key; Size: longword);
var
  i: integer;
begin
burn;
  Size:= Size div 8;

  FillChar(EK,Sizeof(EK),0);
  Move(Key,EK,Size);
  for i:= 0 to 7 do
    EK[i]:= (EK[i] shl 8) or (EK[i] shr 8);
  for i:= 1 to 5 do
  begin
    EK[(i*8)+0]:= (EK[((i-1)*8)+1] shl 9) or (EK[((i-1)*8)+2] shr 7);
    EK[(i*8)+1]:= (EK[((i-1)*8)+2] shl 9) or (EK[((i-1)*8)+3] shr 7);
    EK[(i*8)+2]:= (EK[((i-1)*8)+3] shl 9) or (EK[((i-1)*8)+4] shr 7);
    EK[(i*8)+3]:= (EK[((i-1)*8)+4] shl 9) or (EK[((i-1)*8)+5] shr 7);
    EK[(i*8)+4]:= (EK[((i-1)*8)+5] shl 9) or (EK[((i-1)*8)+6] shr 7);
    EK[(i*8)+5]:= (EK[((i-1)*8)+6] shl 9) or (EK[((i-1)*8)+7] shr 7);
    EK[(i*8)+6]:= (EK[((i-1)*8)+7] shl 9) or (EK[((i-1)*8)+0] shr 7);
    EK[(i*8)+7]:= (EK[((i-1)*8)+0] shl 9) or (EK[((i-1)*8)+1] shr 7);
  end;
  EK[48]:= (EK[41] shl 9) or (EK[42] shr 7);
  EK[49]:= (EK[42] shl 9) or (EK[43] shr 7);
  EK[50]:= (EK[43] shl 9) or (EK[44] shr 7);
  EK[51]:= (EK[44] shl 9) or (EK[45] shr 7);

  DK[51]:= MulInv(EK[3]);
  DK[50]:= -EK[2];
  DK[49]:= -EK[1];
  DK[48]:= MulInv(EK[0]);
  for i:= 0 to 6 do
  begin
    DK[47-i*6]:= EK[i*6+5];
    DK[46-i*6]:= EK[i*6+4];
    DK[45-i*6]:= MulInv(EK[i*6+9]);
    DK[44-i*6]:= -EK[i*6+7];
    DK[43-i*6]:= -EK[i*6+8];
    DK[42-i*6]:= MulInv(EK[i*6+6]);
  end;
  DK[5]:= EK[47];
  DK[4]:= EK[46];
  DK[3]:= MulInv(EK[51]);
  DK[2]:= -EK[50];
  DK[1]:= -EK[49];
  DK[0]:= MulInv(EK[48]);
end;

procedure TIDEA.Burn;
begin
  FillChar(EK,Sizeof(EK),0);
  FillChar(DK,Sizeof(DK),0);
  inherited Burn;
end;

procedure Mul(var x: word; const y: word);
var
  p: DWord;
  t16: word;
begin
  p:= DWord(x)*y;
  if p= 0 then
    x:= 1 - x - y
  else
  begin
    x:= p shr 16;
    t16:= p and $FFFF;
    x:= t16 - x;
    if (t16 < x) then
      Inc(x);
  end;
end;

procedure TIDEA.EncryptECB(const InData; var OutData);
var
  x: array[1..4] of word;
  s3, s2: word;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  PDword(@X[1])^:= PDword(@InData)^;
  PDword(@X[3])^:= PDword(dword(@InData)+4)^;
  for i:= 1 to 4 do
    x[i]:= (x[i] shl 8) or (x[i] shr 8);
  for i:= 0 to 7 do
  begin
    Mul(x[1],EK[(i*6)+0]);
    Inc(x[2],EK[(i*6)+1]);
    Inc(x[3],EK[(i*6)+2]);
    Mul(x[4],EK[(i*6)+3]);
    s3:= x[3];
    x[3]:= x[3] xor x[1];
    Mul(x[3],EK[(i*6)+4]);
    s2:= x[2];
    x[2]:= x[2] xor x[4];
    Inc(x[2],x[3]);
    Mul(x[2],EK[(i*6)+5]);
    Inc(x[3],x[2]);
    x[1]:= x[1] xor x[2];
    x[4]:= x[4] xor x[3];
    x[2]:= x[2] xor s3;
    x[3]:= x[3] xor s2;
  end;
  Mul(x[1],EK[48]);
  Inc(x[3],EK[49]);
  Inc(x[2],EK[50]);
  Mul(x[4],EK[51]);
  x[1]:= (x[1] shl 8) or (x[1] shr 8);
  s2:= (x[3] shl 8) or (x[3] shr 8);
  x[3]:= (x[2] shl 8) or (x[2] shr 8);
  x[4]:= (x[4] shl 8) or (x[4] shr 8);
  x[2]:= s2;
  PDword(@OutData)^:= PDword(@x[1])^;
  PDword(dword(@OutData)+4)^:= PDword(@x[3])^;
end;

procedure TIDEA.DecryptECB(const InData; var OutData);
var
  x: array[1..4] of word;
  s3, s2: word;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  PDword(@X[1])^:= PDword(@InData)^;
  PDword(@X[3])^:= PDword(dword(@InData)+4)^;
  for i:= 1 to 4 do
    x[i]:= (x[i] shl 8) or (x[i] shr 8);
  for i:= 0 to 7 do
  begin
    Mul(x[1],DK[(i*6)+0]);
    Inc(x[2],DK[(i*6)+1]);
    Inc(x[3],DK[(i*6)+2]);
    Mul(x[4],DK[(i*6)+3]);
    s3:= x[3];
    x[3]:= x[3] xor x[1];
    Mul(x[3],DK[(i*6)+4]);
    s2:= x[2];
    x[2]:= x[2] xor x[4];
    Inc(x[2],x[3]);
    Mul(x[2],DK[(i*6)+5]);
    Inc(x[3],x[2]);
    x[1]:= x[1] xor x[2];
    x[4]:= x[4] xor x[3];
    x[2]:= x[2] xor s3;
    x[3]:= x[3] xor s2;
  end;
  Mul(x[1],DK[48]);
  Inc(x[3],DK[49]);
  Inc(x[2],DK[50]);
  Mul(x[4],DK[51]);
  x[1]:= (x[1] shl 8) or (x[1] shr 8);
  s2:= (x[3] shl 8) or (x[3] shr 8);
  x[3]:= (x[2] shl 8) or (x[2] shr 8);
  x[4]:= (x[4] shl 8) or (x[4] shr 8);
  x[2]:= s2;
  PDword(@OutData)^:= PDword(@x[1])^;
  PDword(dword(@OutData)+4)^:= PDword(@x[3])^;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TMisty1.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewMisty1;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////
//{$I DCPmisty1.inc}

function SwapDword(a: dword): dword;
begin
  Result:= ((a and $FF) shl 24) or ((a and $FF00) shl 8) or ((a and $FF0000) shr 8) or ((a and $FF000000) shr 24);
end;

function TMisty1.FI(const FI_IN, FI_KEY: DWord): DWord;
var
  d7, d9: DWord;
begin
  d9:= (FI_IN shr 7) and $1ff;
  d7:= FI_IN and $7f;
  d9:= S9Table[d9] xor d7;
  d7:= (S7Table[d7] xor d9) and $7f;
  d7:= d7 xor ((FI_KEY shr 9) and $7f);
  d9:= d9 xor (FI_KEY and $1ff);
  d9:= S9Table[d9] xor d7;
  Result:= (d7 shl 9) or d9;
end;

function TMisty1.FO(const FO_IN: DWord; const k: longword): DWord;
var
  t0, t1: DWord;
begin
  t0:= FO_IN shr 16;
  t1:= FO_IN and $FFFF;
  t0:= t0 xor KeyData[k];
  t0:= FI(t0,KeyData[((k+5) mod 8) + 8]);
  t0:= t0 xor t1;
  t1:= t1 xor KeyData[(k+2) mod 8];
  t1:= FI(t1,KeyData[((k+1) mod 8) + 8]);
  t1:= t1 xor t0;
  t0:= t0 xor KeyData[(k+7) mod 8];
  t0:= FI(t0,KeyData[((k+3) mod 8) + 8]);
  t0:= t0 xor t1;
  t1:= t1 xor KeyData[(k+4) mod 8];
  Result:= (t1 shl 16) or t0;
end;

function TMisty1.FL(const FL_IN: DWord; const k: longword): DWord;
var
  d0, d1: DWord;
  t: byte;
begin
  d0:= FL_IN shr 16;
  d1:= FL_IN and $FFFF;
  if (k mod 2)<> 0 then
  begin
    t:= (k-1) div 2;
    d1:= d1 xor (d0 and KeyData[((t + 2) mod 8) + 8]);
    d0:= d0 xor (d1 or KeyData[(t + 4) mod 8]);
  end
  else
  begin
    t:= k div 2;
    d1:= d1 xor (d0 and KeyData[t]);
    d0:= d0 xor (d1 or KeyData[((t+6) mod 8) + 8]);
  end;
  Result:= (d0 shl 16) or d1;
end;

function TMisty1.FLINV(const FL_IN: DWord; const k: longword): DWord;
var
  d0, d1: DWord;
  t: byte;
begin
  d0:= FL_IN shr 16;
  d1:= FL_IN and $FFFF;
  if (k mod 2)<> 0 then
  begin
    t:= (k-1) div 2;
    d0:= d0 xor (d1 or KeyData[(t+4) mod 8]);
    d1:= d1 xor (d0 and KeyData[((t+2) mod 8) + 8]);
  end
  else
  begin
    t:= k div 2;
    d0:= d0 xor (d1 or KeyData[((t+6) mod 8) + 8]);
    d1:= d1 xor (d0 and KeyData[t]);
  end;
  Result:= (d0 shl 16) or d1;
end;

procedure TMisty1.InitKey(const Key; Size: longword);
var
  KeyB: array[0..15] of byte;
  i: longword;
begin
burn;
  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size div 8);
  for i:= 0 to 7 do
    KeyData[i]:= (KeyB[i*2] * 256) + KeyB[i*2+1];
  for i:= 0 to 7 do
  begin
    KeyData[i+8]:= FI(KeyData[i],KeyData[(i+1) mod 8]);
    KeyData[i+16]:= KeyData[i+8] and $1FF;
    KeyData[i+24]:= KeyData[i+8] shr 9;
  end;
end;

procedure TMisty1.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TMisty1.EncryptECB(const InData; var OutData);
var
  d0, d1: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  d0:= SwapDWord(PDWord(@InData)^);
  d1:= SwapDWord(PDWord(longword(@InData)+4)^);
  for i:= 0 to NUMROUNDSMY1-1 do
  begin
    if (i mod 2)= 0 then
    begin
      d0:= FL(D0,i);
      d1:= FL(D1,i+1);
      d1:= d1 xor FO(d0,i);
    end
    else
      d0:= d0 xor FO(d1,i);
  end;
  d0:= FL(d0,NUMROUNDSMY1);
  d1:= FL(d1,NUMROUNDSMY1+1);
  PDWord(@OutData)^:= SwapDWord(d1);
  PDWord(longword(@OutData)+4)^:= SwapDWord(d0);
end;

procedure TMisty1.DecryptECB(const InData; var OutData);
var
  d0, d1: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  d1:= SwapDWord(PDWord(@InData)^);
  d0:= SwapDWord(PDWord(longword(@InData)+4)^);
  d1:= FLINV(d1,NUMROUNDSMY1+1);
  d0:= FLINV(d0,NUMROUNDSMY1);
  for i:= NUMROUNDSMY1-1 downto 0 do
  begin
    if (i mod 2)= 0 then
    begin
      d1:= d1 xor FO(d0,i);
      d0:= FLINV(D0,i);
      d1:= FLINV(D1,i+1);
    end
    else
      d0:= d0 xor FO(d1,i);
  end;
  PDWord(@OutData)^:= SwapDWord(d0);
  PDWord(longword(@OutData)+4)^:= SwapDWord(d1);
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TRC2.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewRC2;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////


function LRot16(a, n: word): word;
begin
  Result:= (a shl n) or (a shr (16-n));
end;

function RRot16(a, n: word): word;
begin
  Result:= (a shr n) or (a shl (16-n));
end;

procedure TRC2.InitKey(const Key; Size: longword);
var
  i: longword;
  KeyB: array[0..127] of byte;
begin
burn;
  Move(Key,KeyB,Size div 8);
  for i:= (Size div 8) to 127 do
    KeyB[i]:= sBoxRC2[(KeyB[i-(Size div 8)]+KeyB[i-1]) and $FF];
  KeyB[0]:= sBoxRC2[KeyB[0]];
  Move(KeyB,KeyData,Sizeof(KeyData));
end;

procedure TRC2.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TRC2.EncryptECB(const InData; var OutData);
var
  i, j: longword;
  w: array[0..3] of word;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Pdword(@w[0])^:= Pdword(@InData)^;
  Pdword(@w[2])^:= Pdword(longword(@InData)+4)^;
  for i:= 0 to 15 do
  begin
    j:= i*4;
    w[0]:= LRot16((w[0]+(w[1] and (not w[3]))+(w[2] and w[3])+KeyData[j+0]),1);
    w[1]:= LRot16((w[1]+(w[2] and (not w[0]))+(w[3] and w[0])+KeyData[j+1]),2);
    w[2]:= LRot16((w[2]+(w[3] and (not w[1]))+(w[0] and w[1])+KeyData[j+2]),3);
    w[3]:= LRot16((w[3]+(w[0] and (not w[2]))+(w[1] and w[2])+KeyData[j+3]),5);
    if (i= 4) or (i= 10) then
    begin
      w[0]:= w[0]+KeyData[w[3] and 63];
      w[1]:= w[1]+KeyData[w[0] and 63];
      w[2]:= w[2]+KeyData[w[1] and 63];
      w[3]:= w[3]+KeyData[w[2] and 63];
    end;
  end;
  Pdword(@OutData)^:= Pdword(@w[0])^;
  Pdword(longword(@OutData)+4)^:= Pdword(@w[2])^;
end;

procedure TRC2.DecryptECB(const InData; var OutData);
var
  i, j: longword;
  w: array[0..3] of word;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Pdword(@w[0])^:= Pdword(@InData)^;
  Pdword(@w[2])^:= Pdword(longword(@InData)+4)^;
  for i:= 15 downto 0 do
  begin
    j:= i*4;
    w[3]:= RRot16(w[3],5)-(w[0] and (not w[2]))-(w[1] and w[2])-KeyData[j+3];
    w[2]:= RRot16(w[2],3)-(w[3] and (not w[1]))-(w[0] and w[1])-KeyData[j+2];
    w[1]:= RRot16(w[1],2)-(w[2] and (not w[0]))-(w[3] and w[0])-KeyData[j+1];
    w[0]:= RRot16(w[0],1)-(w[1] and (not w[3]))-(w[2] and w[3])-KeyData[j+0];
    if (i= 5) or (i= 11) then
    begin
      w[3]:= w[3]-KeyData[w[2] and 63];
      w[2]:= w[2]-KeyData[w[1] and 63];
      w[1]:= w[1]-KeyData[w[0] and 63];
      w[0]:= w[0]-KeyData[w[3] and 63];
    end;
  end;
  Pdword(@OutData)^:= Pdword(@w[0])^;
  Pdword(longword(@OutData)+4)^:= Pdword(@w[2])^;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TRC4.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewRC4;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////

procedure Trc4.InitKey(const Key; Size: longword; InitVector: pointer);
var
  i, j, t: longword;
  xKey: array[0..255] of byte;
begin
//  if fInitialized then
    Burn;
//  inherited Init(Key,Size,nil);
  Size:= Size div 8;
  i:= 0;
  while i< 255 do
  begin
    KeyData[i]:= i;
    xKey[i]:= PByte(longword(@Key)+(i mod Size))^;
    KeyData[i+1]:= i+1;
    xKey[i+1]:= PByte(longword(@Key)+((i+1) mod Size))^;
    KeyData[i+2]:= i+2;
    xKey[i+2]:= PByte(longword(@Key)+((i+2) mod Size))^;
    KeyData[i+3]:= i+3;
    xKey[i+3]:= PByte(longword(@Key)+((i+3) mod Size))^;
    KeyData[i+4]:= i+4;
    xKey[i+4]:= PByte(longword(@Key)+((i+4) mod Size))^;
    KeyData[i+5]:= i+5;
    xKey[i+5]:= PByte(longword(@Key)+((i+5) mod Size))^;
    KeyData[i+6]:= i+6;
    xKey[i+6]:= PByte(longword(@Key)+((i+6) mod Size))^;
    KeyData[i+7]:= i+7;
    xKey[i+7]:= PByte(longword(@Key)+((i+7) mod Size))^;
    Inc(i,8);
  end;
  j:= 0;
  i:= 0;
  while i< 255 do
  begin
    j:= (j+KeyData[i]+xKey[i]) and $FF;
    t:= KeyData[i];
    KeyData[i]:= KeyData[j];
    KeyData[j]:= t;
    j:= (j+KeyData[i+1]+xKey[i+1]) and $FF;
    t:= KeyData[i+1];
    KeyData[i+1]:= KeyData[j];
    KeyData[j]:= t;
    j:= (j+KeyData[i+2]+xKey[i+2]) and $FF;
    t:= KeyData[i+2];
    KeyData[i+2]:= KeyData[j];
    KeyData[j]:= t;
    j:= (j+KeyData[i+3]+xKey[i+3]) and $FF;
    t:= KeyData[i+3];
    KeyData[i+3]:= KeyData[j];
    KeyData[j]:= t;
    j:= (j+KeyData[i+4]+xKey[i+4]) and $FF;
    t:= KeyData[i+4];
    KeyData[i+4]:= KeyData[j];
    KeyData[j]:= t;
    j:= (j+KeyData[i+5]+xKey[i+5]) and $FF;
    t:= KeyData[i+5];
    KeyData[i+5]:= KeyData[j];
    KeyData[j]:= t;
    j:= (j+KeyData[i+6]+xKey[i+6]) and $FF;
    t:= KeyData[i+6];
    KeyData[i+6]:= KeyData[j];
    KeyData[j]:= t;
    j:= (j+KeyData[i+7]+xKey[i+7]) and $FF;
    t:= KeyData[i+7];
    KeyData[i+7]:= KeyData[j];
    KeyData[j]:= t;
    Inc(i,8);
  end;
  Move(KeyData,KeyOrg,Sizeof(KeyOrg));
end;

procedure Trc4.Reset;
begin
  Move(KeyOrg,KeyData,Sizeof(KeyData));
end;

procedure Trc4.Burn;
begin
  FillChar(KeyOrg,Sizeof(KeyOrg),$FF);
  FillChar(KeyData,Sizeof(KeyData),$FF);
//  inherited Burn;
end;

procedure Trc4.Encrypt(const InData; var OutData; Size: longword);
var
  i, j, t, k: longword;
begin
//  if not fInitialized then
//    raise EDCP_cipher.Create('Cipher not initialized');
  i:= 0; j:= 0;
  for k:= 0 to Size-1 do
  begin
    i:= (i + 1) and $FF;
    t:= KeyData[i];
    j:= (j + t) and $FF;
    KeyData[i]:= KeyData[j];
    KeyData[j]:= t;
    t:= (t + KeyData[i]) and $FF;
    Pbytearray(@OutData)^[k]:= Pbytearray(@InData)^[k] xor KeyData[t];
  end;
end;

procedure Trc4.Decrypt(const InData; var OutData; Size: longword);
var
  i, j, t, k: longword;
begin
//  if not fInitialized then
//    raise EDCP_cipher.Create('Cipher not initialized');
  i:= 0; j:= 0;
  for k:= 0 to Size-1 do
  begin
    i:= (i + 1) and $FF;
    t:= KeyData[i];
    j:= (j + t) and $FF;
    KeyData[i]:= KeyData[j];
    KeyData[j]:= t;
    t:= (t + KeyData[i]) and $FF;
    Pbytearray(@OutData)^[k]:= Pbytearray(@InData)^[k] xor KeyData[t];
  end;
end;



{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TRC5.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewRC5;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////




function RRot32(a, b: longword): longword;
begin
  Result:= (a shr b) or (a shl (32-b));
end;

procedure TRC5.InitKey(const Key; Size: longword);
var
  xKeyD: array[0..63] of DWord;
  i, j, k, xKeyLen: longword;
  A, B: DWord;
begin
burn;
  FillChar(xKeyD,Sizeof(xKeyD),0);
  Size:= Size div 8;
  Move(Key,xKeyD,Size);
  xKeyLen:= Size div 4;
  if (Size mod 4)<> 0 then
    Inc(xKeyLen);
  Move(sBoxRC5,KeyData,(NUMROUNDSRC5+1)*8);
  i:= 0; j:= 0;
  A:= 0; B:= 0;
  if xKeyLen> ((NUMROUNDSRC5+1)*2) then
    k:= xKeyLen*3
  else
    k:= (NUMROUNDSRC5+1)*6;
  for k:= k downto 1 do
  begin
    A:= LRot32(KeyData[i]+A+B,3);
    KeyData[i]:= A;
    B:= LRot32(xKeyD[j]+A+B,A+B);
    xKeyD[j]:= B;
    i:= (i+1) mod ((NUMROUNDSRC5+1)*2);
    j:= (j+1) mod xKeyLen;
  end;
  FillChar(xKeyD,Sizeof(xKeyD),0);
end;

procedure TRC5.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),$FF);
  inherited Burn;
end;

procedure TRC5.EncryptECB(const InData; var OutData);
var
  A, B: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  A:= PDword(@InData)^ + KeyData[0];
  B:= PDword(longword(@InData)+4)^ + KeyData[1];
  for i:= 1 to NUMROUNDSRC5 do
  begin
    A:= A xor B;
    A:= LRot32(A,B)+KeyData[2*i];
    B:= B xor A;
    B:= LRot32(B,A)+KeyData[(2*i)+1];
  end;
  PDword(@OutData)^:= A;
  PDword(longword(@OutData)+4)^:= B;
end;

procedure TRC5.DecryptECB(const InData; var OutData);
var
  A, B: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  A:= PDword(@InData)^;
  B:= PDword(longword(@InData)+4)^;
  for i:= NUMROUNDSRC5 downto 1 do
  begin
    B:= RRot32(B-KeyData[(2*i)+1],A);
    B:= B xor A;
    A:= RRot32(A-KeyData[2*i],B);
    A:= A xor B;
  end;
  PDword(@OutData)^:= A - KeyData[0];
  PDword(longword(@OutData)+4)^:= B - KeyData[1];
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TTEA.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewTEA;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TTEA.InitKey(const Key; Size: longword);
begin
burn;
  FillChar(KeyData,Sizeof(KeyData),0);
  Move(Key,KeyData,Size div 8);
  KeyData[0]:= SwapDWord(KeyData[0]); KeyData[1]:= SwapDWord(KeyData[1]);
  KeyData[2]:= SwapDWord(KeyData[2]); KeyData[3]:= SwapDWord(KeyData[3]);
end;

procedure TTEA.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TTEA.EncryptECB(const InData; var OutData);
var
  a, b, c, d, x, y, n, sum: dword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');

  x:= SwapDWord(pdword(@InData)^);
  y:= SwapDWord(pdword(longword(@InData)+4)^);
  sum:= 0; a:= KeyData[0]; b:= KeyData[1]; c:= KeyData[2]; d:= KeyData[3];
  for n:= 1 to Rounds do
  begin
    Inc(sum,Delta);
    Inc(x,(y shl 4) + (a xor y) + (sum xor (y shr 5)) + b);
    Inc(y,(x shl 4) + (c xor x) + (sum xor (x shr 5)) + d);
  end;
  pdword(@OutData)^:= SwapDWord(x);
  pdword(longword(@OutData)+4)^:= SwapDWord(y);
end;

procedure TTEA.DecryptECB(const InData; var OutData);
var
  a, b, c, d, x, y, n, sum: dword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');

  x:= SwapDWord(pdword(@InData)^);
  y:= SwapDWord(pdword(longword(@InData)+4)^);
  sum:= Delta shl 5; a:= KeyData[0]; b:= KeyData[1]; c:= KeyData[2]; d:= KeyData[3];
  for n:= 1 to Rounds do
  begin
    Dec(y,(x shl 4) + (c xor x) + (sum xor (x shr 5)) + d);
    Dec(x,(y shl 4) + (a xor y) + (sum xor (y shr 5)) + b);
    Dec(sum,Delta);
  end;
  pdword(@OutData)^:= SwapDWord(x);
  pdword(longword(@OutData)+4)^:= SwapDWord(y);
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TDES.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewDES;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////

procedure hperm_op(var a, t: dword; n, m: dword);
begin
  t:= ((a shl (16 - n)) xor a) and m;
  a:= a xor t xor (t shr (16 - n));
end;

procedure perm_op(var a, b, t: dword; n, m: dword);
begin
  t:= ((a shr n) xor b) and m;
  b:= b xor t;
  a:= a xor (t shl n);
end;

procedure Tcustomdes.DoInit(KeyB: PByteArray; KeyData: PDwordArray);
var
  c, d, t, s, t2, i: dword;
begin
  c:= KeyB^[0] or (KeyB^[1] shl 8) or (KeyB^[2] shl 16) or (KeyB^[3] shl 24);
  d:= KeyB^[4] or (KeyB^[5] shl 8) or (KeyB^[6] shl 16) or (KeyB^[7] shl 24);
  perm_op(d,c,t,4,$0f0f0f0f);
  hperm_op(c,t,dword(-2),$cccc0000);
  hperm_op(d,t,dword(-2),$cccc0000);
  perm_op(d,c,t,1,$55555555);
  perm_op(c,d,t,8,$00ff00ff);
  perm_op(d,c,t,1,$55555555);
  d:= ((d and $ff) shl 16) or (d and $ff00) or ((d and $ff0000) shr 16) or
        ((c and $f0000000) shr 4);
  c:= c and $fffffff;
  for i:= 0 to 15 do
  begin
    if shifts2[i]<> 0 then
    begin
      c:= ((c shr 2) or (c shl 26));
      d:= ((d shr 2) or (d shl 26));
    end
    else
    begin
      c:= ((c shr 1) or (c shl 27));
      d:= ((d shr 1) or (d shl 27));
    end;
    c:= c and $fffffff;
    d:= d and $fffffff;
    s:= des_skb[0,c and $3f] or
        des_skb[1,((c shr  6) and $03) or ((c shr  7) and $3c)] or
        des_skb[2,((c shr 13) and $0f) or ((c shr 14) and $30)] or
        des_skb[3,((c shr 20) and $01) or ((c shr 21) and $06) or ((c shr 22) and $38)];
    t:= des_skb[4,d and $3f] or
        des_skb[5,((d shr  7) and $03) or ((d shr  8) and $3c)] or
        des_skb[6, (d shr 15) and $3f                         ] or
        des_skb[7,((d shr 21) and $0f) or ((d shr 22) and $30)];
    t2:= ((t shl 16) or (s and $ffff));
    KeyData^[(i shl 1)]:= ((t2 shl 2) or (t2 shr 30));
    t2:= ((s shr 16) or (t and $ffff0000));
    KeyData^[(i shl 1)+1]:= ((t2 shl 6) or (t2 shr 26));
  end;
end;

procedure Tcustomdes.EncryptBlock(const InData; var OutData; KeyData: PDWordArray);
var
  l, r, t, u: dword;
  i: longint;
begin
  r:= PDword(@InData)^;
  l:= PDword(dword(@InData)+4)^;
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 0;
  while i< 32 do
  begin
    u:= r xor KeyData^[i  ];
    t:= r xor KeyData^[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i+2];
    t:= l xor KeyData^[i+3];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData^[i+4];
    t:= r xor KeyData^[i+5];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i+6];
    t:= l xor KeyData^[i+7];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Inc(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  PDword(@OutData)^:= l;
  PDword(dword(@OutData)+4)^:= r;
end;

procedure Tcustomdes.DecryptBlock(const InData; var OutData; KeyData: PDWordArray);
var
  l, r, t, u: dword;
  i: longint;
begin
  r:= PDword(@InData)^;
  l:= PDword(dword(@InData)+4)^;
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 30;
  while i> 0 do
  begin
    u:= r xor KeyData^[i  ];
    t:= r xor KeyData^[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i-2];
    t:= l xor KeyData^[i-1];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData^[i-4];
    t:= r xor KeyData^[i-3];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i-6];
    t:= l xor KeyData^[i-5];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Dec(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  PDword(@OutData)^:= l;
  PDword(dword(@OutData)+4)^:= r;
end;

procedure Tdes.InitKey(const Key; Size: longword);
var
  KeyB: array[0..7] of byte;
begin
burn;
  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size div 8);
  DoInit(@KeyB,@KeyData);
end;

procedure Tdes.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure Tdes.EncryptECB(const InData; var OutData);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  EncryptBlock(InData,OutData,@KeyData);
end;

procedure Tdes.DecryptECB(const InData; var OutData);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  DecryptBlock(InData,OutData,@KeyData);
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor T3DES.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function New3DES;
begin
New(Result, Create);
//burn;
// code
end;
////////////////////////////////////////////////////////////////////////////////

procedure T3DES.InitKey(const Key; Size: longword);
var
  KeyB: array[0..2,0..7] of byte;
begin
burn;
  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size div 8);
  DoInit(@KeyB[0],@KeyData[0]);
  DoInit(@KeyB[1],@KeyData[1]);
  if Size> 128 then
    DoInit(@KeyB[2],@KeyData[2])
  else
    Move(KeyData[0],KeyData[2],128);
end;

procedure T3DES.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure T3des.EncryptECB(const InData; var OutData);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  EncryptBlock(InData,OutData,@KeyData[0]);
  DecryptBlock(OutData,OutData,@KeyData[1]);
  EncryptBlock(OutData,OutData,@KeyData[2]);
end;

procedure T3des.DecryptECB(const InData; var OutData);
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  DecryptBlock(InData,OutData,@KeyData[2]);
  EncryptBlock(OutData,OutData,@KeyData[1]);
  DecryptBlock(OutData,OutData,@KeyData[0]);
end;


var
  ice_sbox: array[0..3,0..1023] of dword;
  ice_sboxdone: boolean;

const
  ice_smod: array[0..3,0..3] of dword= (
    (333, 313, 505, 369),
    (379, 375, 319, 391),
    (361, 445, 451, 397),
    (397, 425, 395, 505));
  ice_sxor: array[0..3,0..3] of dword= (
    ($83, $85, $9b, $cd),
    ($cc, $a7, $ad, $41),
    ($4b, $2e, $d4, $33),
    ($ea, $cb, $2e, $04));
  ice_keyrot: array[0..15] of dword= (
     0, 1, 2, 3, 2, 1, 3, 0,
     1, 3, 2, 0, 3, 1, 0, 2);
  ice_pbox: array[0..31] of dword= (
     $00000001,  $00000080,  $00000400,  $00002000,
     $00080000,  $00200000,  $01000000,  $40000000,
     $00000008,  $00000020,  $00000100,  $00004000,
     $00010000,  $00800000,  $04000000,  $20000000,
     $00000004,  $00000010,  $00000200,  $00008000,
     $00020000,  $00400000,  $08000000,  $10000000,
     $00000002,  $00000040,  $00000800,  $00001000,
     $00040000,  $00100000,  $02000000,  $80000000);

{******************************************************************************}
function gf_mult(a, b, m: dword): dword;
var
  res: dword;
begin
  res:= 0;
  while b<> 0 do
  begin
    if (b and 1)<> 0 then
      res:= res xor a;
    a:= a shl 1;
    b:= b shr 1;
    if a>= 256 then
      a:= a xor m;
  end;
  Result:= res;
end;

function gf_exp7(b, m: dword): dword;
var
  x: dword;
begin
  if b= 0 then
    Result:= 0
  else
  begin
    x:= gf_mult(b,b,m);
    x:= gf_mult(b,x,m);
    x:= gf_mult(x,x,m);
    Result:= gf_mult(b,x,m);
  end;
end;

function ice_perm32(x: dword): dword;
var
  res: dword;
  pbox: pdword;
begin
  res:= 0;
  pbox:= @ice_pbox;
  while x<> 0 do
  begin
    if (x and 1)<> 0 then
      res:= res or pbox^;
    Inc(pbox);
    x:= x shr 1;
  end;
  Result:= res;
end;

procedure ice_sboxes_init;
var
  i, col, row: dword;
  x: dword;
begin
  for i:= 0 to 1023 do
  begin
    col:= (i shr 1) and $FF;
    row:= (i and 1) or ((i and $200) shr 8);
    x:= gf_exp7(col xor ice_sxor[0,row],ice_smod[0,row]) shl 24;
    ice_sbox[0,i]:= ice_perm32(x);
    x:= gf_exp7(col xor ice_sxor[1,row],ice_smod[1,row]) shl 16;
    ice_sbox[1,i]:= ice_perm32(x);
    x:= gf_exp7(col xor ice_sxor[2,row],ice_smod[2,row]) shl  8;
    ice_sbox[2,i]:= ice_perm32(x);
    x:= gf_exp7(col xor ice_sxor[3,row],ice_smod[3,row]);
    ice_sbox[3,i]:= ice_perm32(x);
  end;
end;

function Tcustomice.f(p, sk: dword): dword;
var
  tl, tr, al, ar: dword;
begin
  tl:= ((p shr 16) and $3ff) or (((p shr 14) or (p shl 18)) and $ffc00);
  tr:= (p and $3ff) or ((p shl 2) and $ffc00);
  al:= ik_keysched[sk,2] and (tl xor tr);
  ar:= al xor tr;
  al:= al xor tl;
  al:= al xor ik_keysched[sk,0];
  ar:= ar xor ik_keysched[sk,1];
  Result:= ice_sbox[0,al shr 10] or ice_sbox[1,al and $3ff] or
           ice_sbox[2,ar shr 10] or ice_sbox[3,ar and $3ff];
end;


procedure Tcustomice.key_sched_build(kb: pwordarray; n: dword; keyrot: pdwordarray);
var
  i, j, k, kr: dword;
  keys: pdwordarray;
  currentsk: pdword;
  currentkb: pword;
  bit: dword;
begin
  for i:= 0 to 7 do
  begin
    kr:= keyrot^[i];
    keys:= @ik_keysched[n+i];
    for j:= 0 to 2 do
      keys^[j]:= 0;
    for j:= 0 to 14 do
    begin
      currentsk:= @keys^[j mod 3];
      for k:= 0 to 3 do
      begin
        currentkb:= @kb^[(kr + k) and 3];
        bit:= currentkb^ and 1;
        currentsk^:= (currentsk^ shl 1) or bit;
        currentkb^:= (currentkb^ shr 1) or ((bit xor 1) shl 15);
      end;
    end;
  end;
end;

procedure Tcustomice.InitIce(const Key; Size: longword; n: dword);
var
  i, j: dword;
  kb: array[0..3] of word;
  keyb: array[0..15] of byte;
begin
  FillChar(keyb,Sizeof(keyb),0);
  Move(key,keyb,Size div 8);
  if n> 0 then
    rounds:= 16 * n
  else
    rounds:= 8;

  if rounds= 8 then
  begin
    for i:= 0 to 4 do
      kb[3 - i]:= (keyb[i*2] shl 8) or keyb[i*2 + 1];
    key_sched_build(@kb,0,@ice_keyrot);
  end
  else
  begin
    for i:= 0 to (n-1) do
    begin
      for j:= 0 to 3 do
        kb[3-j]:= (keyb[i*8 + j*2] shl 8) or keyb[i*8 + j*2 + 1];
      key_sched_build(@kb,i*8,@ice_keyrot);
      key_sched_build(@kb,rounds - 8 - i*8,@ice_keyrot[8]);
    end;
  end;
end;

procedure Tcustomice.Burn;
begin
  FillChar(ik_keysched,Sizeof(ik_keysched),0);
  Rounds:= 0;
  inherited Burn;
end;

procedure Tcustomice.EncryptECB(const InData; var OutData);
var
  i, l, r: dword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  l:= SwapDWord(Pdword(@InData)^);
  r:= SwapDWord(Pdword(longword(@InData)+4)^);
  i:= 0;
  while i< rounds do
  begin
    l:= l xor f(r,i);
    r:= r xor f(l,i+1);
    Inc(i,2);
  end;
  Pdword(@OutData)^:= SwapDWord(r);
  Pdword(longword(@OutData)+4)^:= SwapDWord(l);
end;

procedure Tcustomice.DecryptECB(const InData; var OutData);
var
  l, r: dword;
  i: integer;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  l:= SwapDWord(Pdword(@InData)^);
  r:= SwapDWord(Pdword(longword(@InData)+4)^);
  i:= rounds-1;
  while i> 0 do
  begin
    l:= l xor f(r,i);
    r:= r xor f(l,i-1);
    Dec(i,2);
  end;
  Pdword(@OutData)^:= SwapDWord(r);
  Pdword(longword(@OutData)+4)^:= SwapDWord(l);
end;



destructor TICE.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewICE;
begin
New(Result, Create);
  if not ice_sboxdone then
  begin
    ice_sboxes_init;
    ice_sboxdone:= true;
  end;
// code
end;

destructor TICE2.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

procedure Tice.InitKey(const Key; Size: longword);
begin
burn;
  InitIce(Key,Size,1);
end;


{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewICE2;
begin
New(Result, Create);
  if not ice_sboxdone then
  begin
    ice_sboxes_init;
    ice_sboxdone:= true;
  end;

// code
end;

destructor TThinICE.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

procedure Tice2.InitKey(const Key; Size: longword);
begin
burn;
  InitIce(Key,Size,2);
end;


{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewThinICE;
begin
New(Result, Create);
  if not ice_sboxdone then
  begin
    ice_sboxes_init;
    ice_sboxdone:= true;
  end;

// code
end;

procedure Tthinice.InitKey(const Key; Size: longword);
begin
burn;
  InitIce(Key,Size,0);
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TCast256.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewCast256;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////

function F1(a,rk,mk: DWord): DWord;
var
  t: DWord;
begin
  t:= LRot32(mk + a,rk);
  Result:= ((S1[t shr 24] xor S2[(t shr 16) and $FF]) - S3[(t shr 8) and $FF]) + S4[t and $FF];
end;
function F2(a,rk,mk: DWord): DWord;
var
  t: DWord;
begin
  t:= LRot32(mk xor a,rk);
  Result:= ((S1[t shr 24] - S2[(t shr 16) and $FF]) + S3[(t shr 8) and $FF]) xor S4[t and $FF];
end;
function F3(a,rk,mk: DWord): DWord;
var
  t: DWord;
begin
  t:= LRot32(mk - a,rk);
  Result:= ((S1[t shr 24] + S2[(t shr 16) and $FF]) xor S3[(t shr 8) and $FF]) - S4[t and $FF];
end;


procedure Tcast256.InitKey(const Key; Size: longword);
var
  x: array[0..7] of DWord;
  cm, cr: DWord;
  i, j: longword;
  tr, tm: array[0..7] of DWord;
begin
burn;
  Size:= Size div 8;

  FillChar(x,Sizeof(x),0);
  Move(Key,x,Size);

  cm:= $5a827999;
  cr:= 19;
  for i:= 0 to 7 do
    x[i]:= (x[i] shl 24) or ((x[i] shl 8) and $FF0000) or ((x[i] shr 8) and $FF00) or (x[i] shr 24);
  for i:= 0 to 11 do
  begin
    for j:= 0 to 7 do
    begin
      tm[j]:= cm;
      Inc(cm,$6ed9eba1);
      tr[j]:= cr;
      Inc(cr,17);
    end;
    x[6]:= x[6] xor f1(x[7],tr[0],tm[0]);
    x[5]:= x[5] xor f2(x[6],tr[1],tm[1]);
    x[4]:= x[4] xor f3(x[5],tr[2],tm[2]);
    x[3]:= x[3] xor f1(x[4],tr[3],tm[3]);
    x[2]:= x[2] xor f2(x[3],tr[4],tm[4]);
    x[1]:= x[1] xor f3(x[2],tr[5],tm[5]);
    x[0]:= x[0] xor f1(x[1],tr[6],tm[6]);
    x[7]:= x[7] xor f2(x[0],tr[7],tm[7]);

    for j:= 0 to 7 do
    begin
      tm[j]:= cm;
      Inc(cm,$6ed9eba1);
      tr[j]:= cr;
      Inc(cr,17);
    end;
    x[6]:= x[6] xor f1(x[7],tr[0],tm[0]);
    x[5]:= x[5] xor f2(x[6],tr[1],tm[1]);
    x[4]:= x[4] xor f3(x[5],tr[2],tm[2]);
    x[3]:= x[3] xor f1(x[4],tr[3],tm[3]);
    x[2]:= x[2] xor f2(x[3],tr[4],tm[4]);
    x[1]:= x[1] xor f3(x[2],tr[5],tm[5]);
    x[0]:= x[0] xor f1(x[1],tr[6],tm[6]);
    x[7]:= x[7] xor f2(x[0],tr[7],tm[7]);

    Kr[i,0]:= x[0] and 31;
    Kr[i,1]:= x[2] and 31;
    Kr[i,2]:= x[4] and 31;
    Kr[i,3]:= x[6] and 31;
    Km[i,0]:= x[7];
    Km[i,1]:= x[5];
    Km[i,2]:= x[3];
    Km[i,3]:= x[1];
  end;
  FillChar(x,Sizeof(x),$FF);
end;

procedure Tcast256.Burn;
begin
  FillChar(Kr,Sizeof(Kr),$FF);
  FillChar(Km,Sizeof(Km),$FF);
  inherited Burn;
end;

procedure Tcast256.EncryptECB(const InData; var OutData);
var
  A: array[0..3] of DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  A[0]:= PDWord(@InData)^;
  A[1]:= PDWord(longword(@InData)+4)^;
  A[2]:= PDWord(longword(@InData)+8)^;
  A[3]:= PDWord(longword(@InData)+12)^;

  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);
  A[2]:= A[2] xor f1(A[3],kr[0,0],km[0,0]);
  A[1]:= A[1] xor f2(A[2],kr[0,1],km[0,1]);
  A[0]:= A[0] xor f3(A[1],kr[0,2],km[0,2]);
  A[3]:= A[3] xor f1(A[0],kr[0,3],km[0,3]);
  A[2]:= A[2] xor f1(A[3],kr[1,0],km[1,0]);
  A[1]:= A[1] xor f2(A[2],kr[1,1],km[1,1]);
  A[0]:= A[0] xor f3(A[1],kr[1,2],km[1,2]);
  A[3]:= A[3] xor f1(A[0],kr[1,3],km[1,3]);
  A[2]:= A[2] xor f1(A[3],kr[2,0],km[2,0]);
  A[1]:= A[1] xor f2(A[2],kr[2,1],km[2,1]);
  A[0]:= A[0] xor f3(A[1],kr[2,2],km[2,2]);
  A[3]:= A[3] xor f1(A[0],kr[2,3],km[2,3]);
  A[2]:= A[2] xor f1(A[3],kr[3,0],km[3,0]);
  A[1]:= A[1] xor f2(A[2],kr[3,1],km[3,1]);
  A[0]:= A[0] xor f3(A[1],kr[3,2],km[3,2]);
  A[3]:= A[3] xor f1(A[0],kr[3,3],km[3,3]);
  A[2]:= A[2] xor f1(A[3],kr[4,0],km[4,0]);
  A[1]:= A[1] xor f2(A[2],kr[4,1],km[4,1]);
  A[0]:= A[0] xor f3(A[1],kr[4,2],km[4,2]);
  A[3]:= A[3] xor f1(A[0],kr[4,3],km[4,3]);
  A[2]:= A[2] xor f1(A[3],kr[5,0],km[5,0]);
  A[1]:= A[1] xor f2(A[2],kr[5,1],km[5,1]);
  A[0]:= A[0] xor f3(A[1],kr[5,2],km[5,2]);
  A[3]:= A[3] xor f1(A[0],kr[5,3],km[5,3]);

  A[3]:= A[3] xor f1(A[0],kr[6,3],km[6,3]);
  A[0]:= A[0] xor f3(A[1],kr[6,2],km[6,2]);
  A[1]:= A[1] xor f2(A[2],kr[6,1],km[6,1]);
  A[2]:= A[2] xor f1(A[3],kr[6,0],km[6,0]);
  A[3]:= A[3] xor f1(A[0],kr[7,3],km[7,3]);
  A[0]:= A[0] xor f3(A[1],kr[7,2],km[7,2]);
  A[1]:= A[1] xor f2(A[2],kr[7,1],km[7,1]);
  A[2]:= A[2] xor f1(A[3],kr[7,0],km[7,0]);
  A[3]:= A[3] xor f1(A[0],kr[8,3],km[8,3]);
  A[0]:= A[0] xor f3(A[1],kr[8,2],km[8,2]);
  A[1]:= A[1] xor f2(A[2],kr[8,1],km[8,1]);
  A[2]:= A[2] xor f1(A[3],kr[8,0],km[8,0]);
  A[3]:= A[3] xor f1(A[0],kr[9,3],km[9,3]);
  A[0]:= A[0] xor f3(A[1],kr[9,2],km[9,2]);
  A[1]:= A[1] xor f2(A[2],kr[9,1],km[9,1]);
  A[2]:= A[2] xor f1(A[3],kr[9,0],km[9,0]);
  A[3]:= A[3] xor f1(A[0],kr[10,3],km[10,3]);
  A[0]:= A[0] xor f3(A[1],kr[10,2],km[10,2]);
  A[1]:= A[1] xor f2(A[2],kr[10,1],km[10,1]);
  A[2]:= A[2] xor f1(A[3],kr[10,0],km[10,0]);
  A[3]:= A[3] xor f1(A[0],kr[11,3],km[11,3]);
  A[0]:= A[0] xor f3(A[1],kr[11,2],km[11,2]);
  A[1]:= A[1] xor f2(A[2],kr[11,1],km[11,1]);
  A[2]:= A[2] xor f1(A[3],kr[11,0],km[11,0]);
  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);

  PDWord(@OutData)^:= A[0];
  PDWord(longword(@OutData)+4)^:= A[1];
  PDWord(longword(@OutData)+8)^:= A[2];
  PDWord(longword(@OutData)+12)^:= A[3];
end;

procedure Tcast256.DecryptECB(const InData; var OutData);
var
  A: array[0..3] of DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  A[0]:= PDWord(@InData)^;
  A[1]:= PDWord(longword(@InData)+4)^;
  A[2]:= PDWord(longword(@InData)+8)^;
  A[3]:= PDWord(longword(@InData)+12)^;

  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);
  A[2]:= A[2] xor f1(A[3],kr[11,0],km[11,0]);
  A[1]:= A[1] xor f2(A[2],kr[11,1],km[11,1]);
  A[0]:= A[0] xor f3(A[1],kr[11,2],km[11,2]);
  A[3]:= A[3] xor f1(A[0],kr[11,3],km[11,3]);
  A[2]:= A[2] xor f1(A[3],kr[10,0],km[10,0]);
  A[1]:= A[1] xor f2(A[2],kr[10,1],km[10,1]);
  A[0]:= A[0] xor f3(A[1],kr[10,2],km[10,2]);
  A[3]:= A[3] xor f1(A[0],kr[10,3],km[10,3]);
  A[2]:= A[2] xor f1(A[3],kr[9,0],km[9,0]);
  A[1]:= A[1] xor f2(A[2],kr[9,1],km[9,1]);
  A[0]:= A[0] xor f3(A[1],kr[9,2],km[9,2]);
  A[3]:= A[3] xor f1(A[0],kr[9,3],km[9,3]);
  A[2]:= A[2] xor f1(A[3],kr[8,0],km[8,0]);
  A[1]:= A[1] xor f2(A[2],kr[8,1],km[8,1]);
  A[0]:= A[0] xor f3(A[1],kr[8,2],km[8,2]);
  A[3]:= A[3] xor f1(A[0],kr[8,3],km[8,3]);
  A[2]:= A[2] xor f1(A[3],kr[7,0],km[7,0]);
  A[1]:= A[1] xor f2(A[2],kr[7,1],km[7,1]);
  A[0]:= A[0] xor f3(A[1],kr[7,2],km[7,2]);
  A[3]:= A[3] xor f1(A[0],kr[7,3],km[7,3]);
  A[2]:= A[2] xor f1(A[3],kr[6,0],km[6,0]);
  A[1]:= A[1] xor f2(A[2],kr[6,1],km[6,1]);
  A[0]:= A[0] xor f3(A[1],kr[6,2],km[6,2]);
  A[3]:= A[3] xor f1(A[0],kr[6,3],km[6,3]);

  A[3]:= A[3] xor f1(A[0],kr[5,3],km[5,3]);
  A[0]:= A[0] xor f3(A[1],kr[5,2],km[5,2]);
  A[1]:= A[1] xor f2(A[2],kr[5,1],km[5,1]);
  A[2]:= A[2] xor f1(A[3],kr[5,0],km[5,0]);
  A[3]:= A[3] xor f1(A[0],kr[4,3],km[4,3]);
  A[0]:= A[0] xor f3(A[1],kr[4,2],km[4,2]);
  A[1]:= A[1] xor f2(A[2],kr[4,1],km[4,1]);
  A[2]:= A[2] xor f1(A[3],kr[4,0],km[4,0]);
  A[3]:= A[3] xor f1(A[0],kr[3,3],km[3,3]);
  A[0]:= A[0] xor f3(A[1],kr[3,2],km[3,2]);
  A[1]:= A[1] xor f2(A[2],kr[3,1],km[3,1]);
  A[2]:= A[2] xor f1(A[3],kr[3,0],km[3,0]);
  A[3]:= A[3] xor f1(A[0],kr[2,3],km[2,3]);
  A[0]:= A[0] xor f3(A[1],kr[2,2],km[2,2]);
  A[1]:= A[1] xor f2(A[2],kr[2,1],km[2,1]);
  A[2]:= A[2] xor f1(A[3],kr[2,0],km[2,0]);
  A[3]:= A[3] xor f1(A[0],kr[1,3],km[1,3]);
  A[0]:= A[0] xor f3(A[1],kr[1,2],km[1,2]);
  A[1]:= A[1] xor f2(A[2],kr[1,1],km[1,1]);
  A[2]:= A[2] xor f1(A[3],kr[1,0],km[1,0]);
  A[3]:= A[3] xor f1(A[0],kr[0,3],km[0,3]);
  A[0]:= A[0] xor f3(A[1],kr[0,2],km[0,2]);
  A[1]:= A[1] xor f2(A[2],kr[0,1],km[0,1]);
  A[2]:= A[2] xor f1(A[3],kr[0,0],km[0,0]);
  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);

  PDWord(@OutData)^:= A[0];
  PDWord(longword(@OutData)+4)^:= A[1];
  PDWord(longword(@OutData)+8)^:= A[2];
  PDWord(longword(@OutData)+12)^:= A[3];
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TMars.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewMars;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////

procedure gen_mask(var x, m: DWord);
var
  u: DWord;
begin
  u:= x and (x shr 1); u:= u and (u shr 2);
  u:= u and (u shr 4); u:= u and (u shr 1) and (u shr 2);
  m:= u;
  u:= (x xor $FFFFFFFF) and ((x xor $FFFFFFFF) shr 1); u:= u and (u shr 2);
  u:= u and (u shr 4); u:= u and (u shr 1) and (u shr 2);
  u:= u or m;
  m:= (u shl 1) or (u shl 2) or (u shl 3)
       or (u shl 4) or (u shl 5) or (u shl 6)
       or (u shl 7) or (u shl 8);
  m:= (m or u or (u shl 9)) and ((x xor $FFFFFFFF) xor (x shl 1)) and ((x xor $FFFFFFFF) xor (x shr 1));
  m:= m and $FFFFFFFC;
end;

procedure Tmars.InitKey(const Key; Size: longword);
var
  i, j, m, u, w: DWord;
  t: array[-7..39] of DWord;
  KeyB: array[0..39] of DWord;
begin
burn;
  Size:= Size div 8;
  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size);
  Size:= Size div 4;
  Move(vk,t,Sizeof(vk));
  for i:= 0 to 38 do
  begin
    u:= t[i-7] xor t[i-2];
    t[i]:= LRot32(u,3) xor KeyB[i mod DWord(Size)] xor i;
  end;
  t[39]:= Size;
  for j:= 0 to 6 do
  begin
    for i:= 1 to 39 do
    begin
      u:= t[i] + s_box[t[i-1] and $1FF];
      t[i]:= LRot32(u,9);
    end;
    u:= t[0] + s_box[t[39] and $1FF];
    t[0]:= LRot32(u,9);
  end;
  for i:= 0 to 39 do
    KeyData[(7*i) mod 40]:= t[i];
  i:= 5;
  repeat
    u:= s_box[265+(KeyData[i] and $3)];
    j:= KeyData[i+3] and $1f;
    w:= KeyData[i] or $3;
    gen_mask(w,m);
    KeyData[i]:= w xor (LRot32(u,j) and m);
    Inc(i,2);
  until i>= 37;
end;

procedure Tmars.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),$FF);
  inherited Burn;
end;

procedure Tmars.EncryptECB(const InData; var OutData);
var
  l, m, r, t: DWord;
  blk: array[0..3] of DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Blk[0]:= PDWord(@InData)^;
  Blk[1]:= PDWord(longword(@InData)+4)^;
  Blk[2]:= PDWord(longword(@InData)+8)^;
  Blk[3]:= PDWord(longword(@InData)+12)^;

  blk[0]:= blk[0] + KeyData[0]; blk[1]:= blk[1] + KeyData[1];
  blk[2]:= blk[2] + KeyData[2]; blk[3]:= blk[3] + KeyData[3];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 16) and $FF];
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24); blk[0]:= blk[0] + blk[3];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 16) and $FF];
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[1]:= RRot32(blk[1], 24); blk[1]:= blk[1] + blk[2];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 16) and $FF];
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[2]:= RRot32(blk[2], 24);
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 16) and $FF];
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 16) and $FF];
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24); blk[0]:= blk[0] + blk[3];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 16) and $FF];
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[1]:= RRot32(blk[1], 24); blk[1]:= blk[1] + blk[2];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 16) and $FF];
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[2]:= RRot32(blk[2], 24);
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 16) and $FF];
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  m:= blk[0] + KeyData[4];
  r:= LRot32(blk[0],13) * KeyData[5];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[1]:= blk[1] + l;
  blk[2]:= blk[2] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[1] + KeyData[6];
  r:= LRot32(blk[1],13) * KeyData[7];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[2]:= blk[2] + l;
  blk[3]:= blk[3] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[2] + KeyData[8];
  r:= LRot32(blk[2],13) * KeyData[9];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[3]:= blk[3] + l;
  blk[0]:= blk[0] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[3] + KeyData[10];
  r:= LRot32(blk[3],13) * KeyData[11];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[0]:= blk[0] + l;
  blk[1]:= blk[1] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[0] + KeyData[12];
  r:= LRot32(blk[0],13) * KeyData[13];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[1]:= blk[1] + l;
  blk[2]:= blk[2] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[1] + KeyData[14];
  r:= LRot32(blk[1],13) * KeyData[15];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[2]:= blk[2] + l;
  blk[3]:= blk[3] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[2] + KeyData[16];
  r:= LRot32(blk[2],13) * KeyData[17];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[3]:= blk[3] + l;
  blk[0]:= blk[0] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[3] + KeyData[18];
  r:= LRot32(blk[3],13) * KeyData[19];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[0]:= blk[0] + l;
  blk[1]:= blk[1] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[0] + KeyData[20];
  r:= LRot32(blk[0],13) * KeyData[21];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[3]:= blk[3] + l;
  blk[2]:= blk[2] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[1] + KeyData[22];
  r:= LRot32(blk[1],13) * KeyData[23];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[0]:= blk[0] + l;
  blk[3]:= blk[3] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[2] + KeyData[24];
  r:= LRot32(blk[2],13) * KeyData[25];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[1]:= blk[1] + l;
  blk[0]:= blk[0] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[3] + KeyData[26];
  r:= LRot32(blk[3],13) * KeyData[27];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[2]:= blk[2] + l;
  blk[1]:= blk[1] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[0] + KeyData[28];
  r:= LRot32(blk[0],13) * KeyData[29];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[3]:= blk[3] + l;
  blk[2]:= blk[2] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[1] + KeyData[30];
  r:= LRot32(blk[1],13) * KeyData[31];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[0]:= blk[0] + l;
  blk[3]:= blk[3] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[2] + KeyData[32];
  r:= LRot32(blk[2],13) * KeyData[33];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[1]:= blk[1] + l;
  blk[0]:= blk[0] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[3] + KeyData[34];
  r:= LRot32(blk[3],13) * KeyData[35];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[2]:= blk[2] + l;
  blk[1]:= blk[1] + m;
  blk[0]:= blk[0] xor r;
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 24) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[0]:= LRot32(blk[0], 24);
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 24) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[1]:= LRot32(blk[1], 24); blk[2]:= blk[2] - blk[1];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 24) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[2]:= LRot32(blk[2], 24); blk[3]:= blk[3] - blk[0];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 24) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 24) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[0]:= LRot32(blk[0], 24);
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 24) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[1]:= LRot32(blk[1], 24); blk[2]:= blk[2] - blk[1];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 24) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[2]:= LRot32(blk[2], 24); blk[3]:= blk[3] - blk[0];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 24) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[0]:= blk[0] - KeyData[36]; blk[1]:= blk[1] - KeyData[37];
  blk[2]:= blk[2] - KeyData[38]; blk[3]:= blk[3] - KeyData[39];

  PDWord(@OutData)^:= Blk[0];
  PDWord(longword(@OutData)+4)^:= Blk[1];
  PDWord(longword(@OutData)+8)^:= Blk[2];
  PDWord(longword(@OutData)+12)^:= Blk[3];
end;

procedure Tmars.DecryptECB(const InData; var OutData);
var
  l, m, r, t: DWord;
  blk: array[0..3] of DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  Blk[0]:= PDWord(@InData)^;
  Blk[1]:= PDWord(longword(@InData)+4)^;
  Blk[2]:= PDWord(longword(@InData)+8)^;
  Blk[3]:= PDWord(longword(@InData)+12)^;

  blk[0]:= blk[0] + KeyData[36]; blk[1]:= blk[1] + KeyData[37];
  blk[2]:= blk[2] + KeyData[38]; blk[3]:= blk[3] + KeyData[39];
  blk[3]:= RRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 24) and $FF];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[3]:= blk[3] + blk[0]; blk[2]:= RRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 24) and $FF];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[2]:= blk[2] + blk[1]; blk[1]:= RRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 24) and $FF];
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 24) and $FF];
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 24) and $FF];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[3]:= blk[3] + blk[0]; blk[2]:= RRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 24) and $FF];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[2]:= blk[2] + blk[1]; blk[1]:= RRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 24) and $FF];
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 24) and $FF];
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[34];
  r:= LRot32(blk[3],13) * KeyData[35];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[1]:= blk[1] - m;
  blk[0]:= blk[0] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[32];
  r:= LRot32(blk[2],13) * KeyData[33];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[0]:= blk[0] - m;
  blk[3]:= blk[3] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[30];
  r:= LRot32(blk[1],13) * KeyData[31];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[3]:= blk[3] - m;
  blk[2]:= blk[2] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[28];
  r:= LRot32(blk[0],13) * KeyData[29];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[2]:= blk[2] - m;
  blk[1]:= blk[1] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[26];
  r:= LRot32(blk[3],13) * KeyData[27];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[1]:= blk[1] - m;
  blk[0]:= blk[0] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[24];
  r:= LRot32(blk[2],13) * KeyData[25];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[0]:= blk[0] - m;
  blk[3]:= blk[3] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[22];
  r:= LRot32(blk[1],13) * KeyData[23];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[3]:= blk[3] - m;
  blk[2]:= blk[2] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[20];
  r:= LRot32(blk[0],13) * KeyData[21];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[2]:= blk[2] - m;
  blk[1]:= blk[1] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[18];
  r:= LRot32(blk[3],13) * KeyData[19];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[1]:= blk[1] - m;
  blk[2]:= blk[2] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[16];
  r:= LRot32(blk[2],13) * KeyData[17];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[0]:= blk[0] - m;
  blk[1]:= blk[1] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[14];
  r:= LRot32(blk[1],13) * KeyData[15];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[3]:= blk[3] - m;
  blk[0]:= blk[0] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[12];
  r:= LRot32(blk[0],13) * KeyData[13];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[2]:= blk[2] - m;
  blk[3]:= blk[3] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[10];
  r:= LRot32(blk[3],13) * KeyData[11];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[1]:= blk[1] - m;
  blk[2]:= blk[2] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[8];
  r:= LRot32(blk[2],13) * KeyData[9];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[0]:= blk[0] - m;
  blk[1]:= blk[1] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[6];
  r:= LRot32(blk[1],13) * KeyData[7];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[3]:= blk[3] - m;
  blk[0]:= blk[0] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[4];
  r:= LRot32(blk[0],13) * KeyData[5];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[2]:= blk[2] - m;
  blk[3]:= blk[3] xor r;
  blk[3]:= LRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 16) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[2]:= LRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 16) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[1]:= blk[1] - blk[2]; blk[1]:= LRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 16) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[0]:= blk[0] - blk[3]; blk[0]:= LRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 16) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 16) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[2]:= LRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 16) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[1]:= blk[1] - blk[2]; blk[1]:= LRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 16) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[0]:= blk[0] - blk[3]; blk[0]:= LRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 16) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[0]:= blk[0] - KeyData[0]; blk[1]:= blk[1] - KeyData[1];
  blk[2]:= blk[2] - KeyData[2]; blk[3]:= blk[3] - KeyData[3];

  PDWord(@OutData)^:= Blk[0];
  PDWord(longword(@OutData)+4)^:= Blk[1];
  PDWord(longword(@OutData)+8)^:= Blk[2];
  PDWord(longword(@OutData)+12)^:= Blk[3];
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TRC6.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewRC6;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////


procedure Trc6.InitKey(const Key; Size: longword);
var
  xKeyD: array[0..63] of DWord;
  i, j, k, xKeyLen: longword;
  A, B: DWord;
begin
burn;
  Size:= Size div 8;
  FillChar(xKeyD,Sizeof(xKeyD),0);
  Move(Key,xKeyD,Size);
  xKeyLen:= Size div 4;
  if (Size mod 4)<> 0 then
    Inc(xKeyLen);
  Move(SBoxRC6,KeyData,((NUMROUNDSRC6*2)+4)*4);
  i:= 0; j:= 0;
  A:= 0; B:= 0;
  if xKeyLen> ((NUMROUNDSRC6*2)+4) then
    k:= xKeyLen*3
  else
    k:= ((NUMROUNDSRC6*2)+4)*3;
  for k:= 1 to k do
  begin
    A:= LRot32(KeyData[i]+A+B,3);
    KeyData[i]:= A;
    B:= LRot32(xKeyD[j]+A+B,A+B);
    xKeyD[j]:= B;
    i:= (i+1) mod ((NUMROUNDSRC6*2)+4);
    j:= (j+1) mod xKeyLen;
  end;
  FillChar(xKeyD,Sizeof(xKeyD),0);
end;

procedure Trc6.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),$FF);
  inherited Burn;
end;

procedure Trc6.EncryptECB(const InData; var OutData);
var
  x0, x1, x2, x3: DWord;
  u, t: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  x0:= PDword(@InData)^;
  x1:= PDword(longword(@InData)+4)^;
  x2:= PDword(longword(@InData)+8)^;
  x3:= PDword(longword(@InData)+12)^;
  x1:= x1 + KeyData[0];
  x3:= x3 + KeyData[1];
  for i:= 1 to NUMROUNDSRC6 do
  begin
    t:= Lrot32(x1 * (2*x1 + 1),5);
    u:= Lrot32(x3 * (2*x3 + 1),5);
    x0:= Lrot32(x0 xor t,u) + KeyData[2*i];
    x2:= Lrot32(x2 xor u,t) + KeyData[2*i+1];
    t:= x0; x0:= x1; x1:= x2; x2:= x3; x3:= t;
  end;
  x0:= x0 + KeyData[(2*NUMROUNDSRC6)+2];
  x2:= x2 + KeyData[(2*NUMROUNDSRC6)+3];
  PDword(@OutData)^:= x0;
  PDword(longword(@OutData)+4)^:= x1;
  PDword(longword(@OutData)+8)^:= x2;
  PDword(longword(@OutData)+12)^:= x3;
end;

procedure Trc6.DecryptECB(const InData; var OutData);
var
  x0, x1, x2, x3: DWord;
  u, t: DWord;
  i: longword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  x0:= PDword(@InData)^;
  x1:= PDword(longword(@InData)+4)^;
  x2:= PDword(longword(@InData)+8)^;
  x3:= PDword(longword(@InData)+12)^;
  x2:= x2 - KeyData[(2*NUMROUNDSRC6)+3];
  x0:= x0 - KeyData[(2*NUMROUNDSRC6)+2];
  for i:= NUMROUNDSRC6 downto 1 do
  begin
    t:= x0; x0:= x3; x3:= x2; x2:= x1; x1:= t;
    u:= Lrot32(x3 * (2*x3 + 1),5);
    t:= Lrot32(x1 * (2*x1 + 1),5);
    x2:= Rrot32(x2 - KeyData[2*i+1],t) xor u;
    x0:= Rrot32(x0 - KeyData[2*i],u) xor t;
  end;
  x3:= x3 - KeyData[1];
  x1:= x1 - KeyData[0];
  PDword(@OutData)^:= x0;
  PDword(longword(@OutData)+4)^:= x1;
  PDword(longword(@OutData)+8)^:= x2;
  PDword(longword(@OutData)+12)^:= x3;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TRijndael.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewRijndael;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////


procedure InvMixColumn(a: PByteArray; BC: byte);
var
  j: longword;
begin
  for j:= 0 to (BC-1) do
    PDWord(@(a^[j*4]))^:= PDWord(@U1[a^[j*4+0]])^ xor
                       PDWord(@U2[a^[j*4+1]])^ xor
                       PDWord(@U3[a^[j*4+2]])^ xor
                       PDWord(@U4[a^[j*4+3]])^;
end;

procedure Trijndael.InitKey(const Key; Size: longword);
var
  KC, ROUNDS, j, r, t, rconpointer: longword;
  tk: array[0..MAXKC-1,0..3] of byte;
begin
burn;
  Size:= Size div 8;

  FillChar(tk,Sizeof(tk),0);
  Move(Key,tk,Size);
  if Size<= 16 then
  begin
    KC:= 4;
    Rounds:= 10;
  end
  else if Size<= 24 then
  begin
    KC:= 6;
    Rounds:= 12;
  end
  else
  begin
    KC:= 8;
    Rounds:= 14;
  end;
  numrounds:= rounds;
  r:= 0;
  t:= 0;
  j:= 0;
  while (j< KC) and (r< (rounds+1)) do
  begin
    while (j< KC) and (t< BCRJ) do
    begin
      rk[r,t]:= PDWord(@tk[j])^;
      Inc(j);
      Inc(t);
    end;
    if t= BCRJ then
    begin
      t:= 0;
      Inc(r);
    end;
  end;
  rconpointer:= 0;
  while (r< (rounds+1)) do
  begin
    tk[0,0]:= tk[0,0] xor S[tk[KC-1,1]];
    tk[0,1]:= tk[0,1] xor S[tk[KC-1,2]];
    tk[0,2]:= tk[0,2] xor S[tk[KC-1,3]];
    tk[0,3]:= tk[0,3] xor S[tk[KC-1,0]];
    tk[0,0]:= tk[0,0] xor rcon[rconpointer];
    Inc(rconpointer);
    if KC<> 8 then
    begin
      for j:= 1 to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end
    else
    begin
      for j:= 1 to ((KC div 2)-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
      tk[KC div 2,0]:= tk[KC div 2,0] xor S[tk[KC div 2 - 1,0]];
      tk[KC div 2,1]:= tk[KC div 2,1] xor S[tk[KC div 2 - 1,1]];
      tk[KC div 2,2]:= tk[KC div 2,2] xor S[tk[KC div 2 - 1,2]];
      tk[KC div 2,3]:= tk[KC div 2,3] xor S[tk[KC div 2 - 1,3]];
      for j:= ((KC div 2) + 1) to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end;
    j:= 0;
    while (j< KC) and (r< (rounds+1)) do
    begin
      while (j< KC) and (t< BCRJ) do
      begin
        rk[r,t]:= PDWord(@tk[j])^;
        Inc(j);
        Inc(t);
      end;
      if t= BCRJ then
      begin
        Inc(r);
        t:= 0;
      end;
    end;
  end;
  Move(rk,drk,Sizeof(rk));
  for r:= 1 to (numrounds-1) do
    InvMixColumn(@drk[r],BCRJ);
end;

procedure Trijndael.Burn;
begin
  numrounds:= 0;
  FillChar(rk,Sizeof(rk),0);
  FillChar(drk,Sizeof(drk),0);
  inherited Burn;
end;

procedure Trijndael.EncryptECB(const InData; var OutData);
var
  r: longword;
  tempb: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  PDword(@a[0,0])^:= PDword(@InData)^;
  PDword(@a[1,0])^:= PDword(dword(@InData)+4)^;
  PDword(@a[2,0])^:= PDword(dword(@InData)+8)^;
  PDword(@a[3,0])^:= PDword(dword(@InData)+12)^;
  for r:= 0 to (numrounds-2) do
  begin
    PDWord(@tempb[0])^:= PDWord(@a[0])^ xor rk[r,0];
    PDWord(@tempb[1])^:= PDWord(@a[1])^ xor rk[r,1];
    PDWord(@tempb[2])^:= PDWord(@a[2])^ xor rk[r,2];
    PDWord(@tempb[3])^:= PDWord(@a[3])^ xor rk[r,3];
    PDWord(@a[0])^:= PDWord(@T1[tempb[0,0]])^ xor
                     PDWord(@T2[tempb[1,1]])^ xor
                     PDWord(@T3[tempb[2,2]])^ xor
                     PDWord(@T4[tempb[3,3]])^;
    PDWord(@a[1])^:= PDWord(@T1[tempb[1,0]])^ xor
                     PDWord(@T2[tempb[2,1]])^ xor
                     PDWord(@T3[tempb[3,2]])^ xor
                     PDWord(@T4[tempb[0,3]])^;
    PDWord(@a[2])^:= PDWord(@T1[tempb[2,0]])^ xor
                     PDWord(@T2[tempb[3,1]])^ xor
                     PDWord(@T3[tempb[0,2]])^ xor
                     PDWord(@T4[tempb[1,3]])^;
    PDWord(@a[3])^:= PDWord(@T1[tempb[3,0]])^ xor
                     PDWord(@T2[tempb[0,1]])^ xor
                     PDWord(@T3[tempb[1,2]])^ xor
                     PDWord(@T4[tempb[2,3]])^;
  end;
  PDWord(@tempb[0])^:= PDWord(@a[0])^ xor rk[numrounds-1,0];
  PDWord(@tempb[1])^:= PDWord(@a[1])^ xor rk[numrounds-1,1];
  PDWord(@tempb[2])^:= PDWord(@a[2])^ xor rk[numrounds-1,2];
  PDWord(@tempb[3])^:= PDWord(@a[3])^ xor rk[numrounds-1,3];
  a[0,0]:= T1[tempb[0,0],1];
  a[0,1]:= T1[tempb[1,1],1];
  a[0,2]:= T1[tempb[2,2],1];
  a[0,3]:= T1[tempb[3,3],1];
  a[1,0]:= T1[tempb[1,0],1];
  a[1,1]:= T1[tempb[2,1],1];
  a[1,2]:= T1[tempb[3,2],1];
  a[1,3]:= T1[tempb[0,3],1];
  a[2,0]:= T1[tempb[2,0],1];
  a[2,1]:= T1[tempb[3,1],1];
  a[2,2]:= T1[tempb[0,2],1];
  a[2,3]:= T1[tempb[1,3],1];
  a[3,0]:= T1[tempb[3,0],1];
  a[3,1]:= T1[tempb[0,1],1];
  a[3,2]:= T1[tempb[1,2],1];
  a[3,3]:= T1[tempb[2,3],1];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor rk[numrounds,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor rk[numrounds,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor rk[numrounds,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor rk[numrounds,3];

  PDword(@OutData)^:= PDword(@a[0,0])^;
  PDword(dword(@OutData)+4)^:= PDword(@a[1,0])^;
  PDword(dword(@OutData)+8)^:= PDword(@a[2,0])^;
  PDword(dword(@OutData)+12)^:= PDword(@a[3,0])^;
end;

procedure Trijndael.DecryptECB(const InData; var OutData);
var
  r: longword;
  tempb: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  PDword(@a[0,0])^:= PDword(@InData)^;
  PDword(@a[1,0])^:= PDword(dword(@InData)+4)^;
  PDword(@a[2,0])^:= PDword(dword(@InData)+8)^;
  PDword(@a[3,0])^:= PDword(dword(@InData)+12)^;
  for r:= NumRounds downto 2 do
  begin
    PDWord(@tempb[0])^:= PDWord(@a[0])^ xor drk[r,0];
    PDWord(@tempb[1])^:= PDWord(@a[1])^ xor drk[r,1];
    PDWord(@tempb[2])^:= PDWord(@a[2])^ xor drk[r,2];
    PDWord(@tempb[3])^:= PDWord(@a[3])^ xor drk[r,3];
    PDWord(@a[0])^:= PDWord(@T5[tempb[0,0]])^ xor
                     PDWord(@T6[tempb[3,1]])^ xor
                     PDWord(@T7[tempb[2,2]])^ xor
                     PDWord(@T8[tempb[1,3]])^;
    PDWord(@a[1])^:= PDWord(@T5[tempb[1,0]])^ xor
                     PDWord(@T6[tempb[0,1]])^ xor
                     PDWord(@T7[tempb[3,2]])^ xor
                     PDWord(@T8[tempb[2,3]])^;
    PDWord(@a[2])^:= PDWord(@T5[tempb[2,0]])^ xor
                     PDWord(@T6[tempb[1,1]])^ xor
                     PDWord(@T7[tempb[0,2]])^ xor
                     PDWord(@T8[tempb[3,3]])^;
    PDWord(@a[3])^:= PDWord(@T5[tempb[3,0]])^ xor
                     PDWord(@T6[tempb[2,1]])^ xor
                     PDWord(@T7[tempb[1,2]])^ xor
                     PDWord(@T8[tempb[0,3]])^;
  end;
  PDWord(@tempb[0])^:= PDWord(@a[0])^ xor drk[1,0];
  PDWord(@tempb[1])^:= PDWord(@a[1])^ xor drk[1,1];
  PDWord(@tempb[2])^:= PDWord(@a[2])^ xor drk[1,2];
  PDWord(@tempb[3])^:= PDWord(@a[3])^ xor drk[1,3];
  a[0,0]:= S5[tempb[0,0]];
  a[0,1]:= S5[tempb[3,1]];
  a[0,2]:= S5[tempb[2,2]];
  a[0,3]:= S5[tempb[1,3]];
  a[1,0]:= S5[tempb[1,0]];
  a[1,1]:= S5[tempb[0,1]];
  a[1,2]:= S5[tempb[3,2]];
  a[1,3]:= S5[tempb[2,3]];
  a[2,0]:= S5[tempb[2,0]];
  a[2,1]:= S5[tempb[1,1]];
  a[2,2]:= S5[tempb[0,2]];
  a[2,3]:= S5[tempb[3,3]];
  a[3,0]:= S5[tempb[3,0]];
  a[3,1]:= S5[tempb[2,1]];
  a[3,2]:= S5[tempb[1,2]];
  a[3,3]:= S5[tempb[0,3]];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor drk[0,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor drk[0,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor drk[0,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor drk[0,3];
  PDword(@OutData)^:= PDword(@a[0,0])^;
  PDword(dword(@OutData)+4)^:= PDword(@a[1,0])^;
  PDword(dword(@OutData)+8)^:= PDword(@a[2,0])^;
  PDword(dword(@OutData)+12)^:= PDword(@a[3,0])^;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TSerpent.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewSerpent;
begin
New(Result, Create);

// code
end;
////////////////////////////////////////////////////////////////////////////////


procedure Tserpent.InitKey(const Key; Size: longword);
var
  kp: array[0..139] of dword;
  i, n: integer;
  t, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17: dword;
  a, b, c, d: dword;
begin
burn;
  FillChar(kp,256 div 8,0);
  Move(Key,kp,Size div 8);
  if Size < 256 then
  begin
    i:= Size div 32;
    t:= 1 shl (Size mod 32);
    kp[i]:= (kp[i] and (t - 1)) or t;
  end;
  for i:= 8 to 139 do
  begin
    t:= kp[i - 8] xor kp[i - 5] xor kp[i - 3] xor kp[i - 1] xor $9e3779b9 xor longword(i-8);
    kp[i]:= (t shl 11) or (t shr 21);
  end;
  for i:= 0 to 3 do
  begin
    n:= i*32;
    a:= kp[n + 4*0 + 8]; b:= kp[n + 4*0 + 9]; c:= kp[n + 4*0 + 10]; d:= kp[n + 4*0 + 11];
    t1:= a xor c; t2:= a or d; t3:= a and b; t4:= a and d; t5:= b or t4; t6:= t1 and t2; kp[ 9+n]:= t5 xor t6; t8:= b xor d; t9:= c or t3; t10:= t6 xor t8; kp[ 11+n]:= t9 xor t10; t12:= c xor t3; t13:= t2 and kp[ 11+n]; kp[ 10+n]:= t12 xor t13; t15:= not kp[ 10+n]; t16:= t2 xor t3; t17:= kp[ 9+n] and t15; kp[ 8+n]:= t16 xor t17;
    a:= kp[n + 4*1 + 8]; b:= kp[n + 4*1 + 9]; c:= kp[n + 4*1 + 10]; d:= kp[n + 4*1 + 11];
    t1:= not a; t2:= b xor d; t3:= c and t1; kp[ 12+n]:= t2 xor t3; t5:= c xor t1; t6:= c xor kp[ 12+n]; t7:= b and t6; kp[ 15+n]:= t5 xor t7; t9:= d or t7; t10:= kp[ 12+n] or t5; t11:= t9 and t10; kp[ 14+n]:= a xor t11; t13:= d or t1; t14:= t2 xor kp[ 15+n]; t15:= kp[ 14+n] xor t13; kp[ 13+n]:= t14 xor t15;
    a:= kp[n + 4*2 + 8]; b:= kp[n + 4*2 + 9]; c:= kp[n + 4*2 + 10]; d:= kp[n + 4*2 + 11];
    t1:= a xor d; t2:= b xor d; t3:= a and b; t4:= not c; t5:= t2 xor t3; kp[ 18+n]:= t4 xor t5; t7:= a xor t2; t8:= b or t4; t9:= d or kp[ 18+n]; t10:= t7 and t9; kp[ 17+n]:= t8 xor t10; t12:= c xor d; t13:= t1 or t2; t14:= kp[ 17+n] xor t12; kp[ 19+n]:= t13 xor t14; t16:= t1 or kp[ 18+n]; t17:= t8 xor t14; kp[ 16+n]:= t16 xor t17;
    a:= kp[n + 4*3 + 8]; b:= kp[n + 4*3 + 9]; c:= kp[n + 4*3 + 10]; d:= kp[n + 4*3 + 11];
    t1:= b xor d; t2:= not t1; t3:= a or d; t4:= b xor c; kp[ 23+n]:= t3 xor t4; t6:= a xor b; t7:= a or t4; t8:= c and t6; t9:= t2 or t8; kp[ 20+n]:= t7 xor t9; t11:= a xor kp[ 23+n]; t12:= t1 and t6; t13:= kp[ 20+n] xor t11; kp[ 21+n]:= t12 xor t13; t15:= kp[ 20+n] or kp[ 21+n]; t16:= t3 and t15; kp[ 22+n]:= b xor t16;
    a:= kp[n + 4*4 + 8]; b:= kp[n + 4*4 + 9]; c:= kp[n + 4*4 + 10]; d:= kp[n + 4*4 + 11];
    t1:= not c; t2:= b xor c; t3:= b or t1; t4:= d xor t3; t5:= a and t4; kp[ 27+n]:= t2 xor t5; t7:= a xor d; t8:= b xor t5; t9:= t2 or t8; kp[ 25+n]:= t7 xor t9; t11:= d and t3; t12:= t5 xor kp[ 25+n]; t13:= kp[ 27+n] and t12; kp[ 26+n]:= t11 xor t13; t15:= t1 or t4; t16:= t12 xor kp[ 26+n]; kp[ 24+n]:= t15 xor t16;
    a:= kp[n + 4*5 + 8]; b:= kp[n + 4*5 + 9]; c:= kp[n + 4*5 + 10]; d:= kp[n + 4*5 + 11];
    t1:= a xor c; t2:= b or d; t3:= b xor c; t4:= not t3; t5:= a and d; kp[ 29+n]:= t4 xor t5; t7:= b or c; t8:= d xor t1; t9:= t7 and t8; kp[ 31+n]:= t2 xor t9; t11:= t1 and t7; t12:= t4 xor t8; t13:= kp[ 31+n] and t11; kp[ 28+n]:= t12 xor t13; t15:= t3 xor t11; t16:= kp[ 31+n] or t15; kp[ 30+n]:= t12 xor t16;
    a:= kp[n + 4*6 + 8]; b:= kp[n + 4*6 + 9]; c:= kp[n + 4*6 + 10]; d:= kp[n + 4*6 + 11];
    t1:= not a; t2:= a xor b; t3:= a xor d; t4:= c xor t1; t5:= t2 or t3; kp[ 32+n]:= t4 xor t5; t7:= not d; t8:= kp[ 32+n] and t7; kp[ 33+n]:= t2 xor t8; t10:= b or kp[ 33+n]; t11:= c or kp[ 32+n]; t12:= t7 xor t10; kp[ 35+n]:= t11 xor t12; t14:= d or kp[ 33+n]; t15:= t1 xor t14; t16:= kp[ 32+n] or kp[ 35+n]; kp[ 34+n]:= t15 xor t16;
    a:= kp[n + 4*7 + 8]; b:= kp[n + 4*7 + 9]; c:= kp[n + 4*7 + 10]; d:= kp[n + 4*7 + 11];
    t1:= not a; t2:= a xor d; t3:= a xor b; t4:= c xor t1; t5:= t2 or t3; kp[ 36+n]:= t4 xor t5; t7:= not kp[ 36+n]; t8:= b or t7; kp[ 39+n]:= t2 xor t8; t10:= a and kp[ 36+n]; t11:= b xor kp[ 39+n]; t12:= t8 and t11; kp[ 38+n]:= t10 xor t12; t14:= a or t7; t15:= t3 xor t14; t16:= kp[ 39+n] and kp[ 38+n]; kp[ 37+n]:= t15 xor t16;
  end;
  a:= kp[136]; b:= kp[137]; c:= kp[138]; d:= kp[139];
  t1:= a xor c; t2:= a or d; t3:= a and b; t4:= a and d; t5:= b or t4; t6:= t1 and t2; kp[137]:= t5 xor t6; t8:= b xor d; t9:= c or t3; t10:= t6 xor t8; kp[139]:= t9 xor t10; t12:= c xor t3; t13:= t2 and kp[139]; kp[138]:= t12 xor t13; t15:= not kp[138]; t16:= t2 xor t3; t17:= kp[137] and t15; kp[136]:= t16 xor t17;
  Move(kp[8],l_key,Sizeof(l_key));
  FillChar(kp,Sizeof(kp),0);
end;

procedure Tserpent.Burn;
begin
  FillChar(l_key,Sizeof(l_key),0);
  inherited Burn;
end;

procedure Tserpent.EncryptECB(const InData; var OutData);
var
  i: integer;
  a, b, c, d, e, f, g, h: dword;
  t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17: dword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');

  a:= PDWord(@InData)^;
  b:= PDWord(longword(@InData)+4)^;
  c:= PDWord(longword(@InData)+8)^;
  d:= PDWord(longword(@InData)+12)^;

  i:= 0;
  while i < 32 do
  begin
    a:= a xor l_key[4*(i)]; b:= b xor l_key[4*(i)+1]; c:= c xor l_key[4*(i)+2]; d:= d xor l_key[4*(i)+3];
    t1:= b xor d; t2:= not t1; t3:= a or d; t4:= b xor c; h:= t3 xor t4; t6:= a xor b; t7:= a or t4; t8:= c and t6; t9:= t2 or t8; e:= t7 xor t9; t11:= a xor h; t12:= t1 and t6; t13:= e xor t11; f:= t12 xor t13; t15:= e or f; t16:= t3 and t15; g:= b xor t16;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+1)]; f:= f xor l_key[4*(i+1)+1]; g:= g xor l_key[4*(i+1)+2]; h:= h xor l_key[4*(i+1)+3];
    t1:= e xor h; t2:= f xor h; t3:= e and f; t4:= not g; t5:= t2 xor t3; c:= t4 xor t5; t7:= e xor t2; t8:= f or t4; t9:= h or c; t10:= t7 and t9; b:= t8 xor t10; t12:= g xor h; t13:= t1 or t2; t14:= b xor t12; d:= t13 xor t14; t16:= t1 or c; t17:= t8 xor t14; a:= t16 xor t17;
    a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    a:= a xor l_key[4*(i+2)]; b:= b xor l_key[4*(i+2)+1]; c:= c xor l_key[4*(i+2)+2]; d:= d xor l_key[4*(i+2)+3];
    t1:= not a; t2:= b xor d; t3:= c and t1; e:= t2 xor t3; t5:= c xor t1; t6:= c xor e; t7:= b and t6; h:= t5 xor t7; t9:= d or t7; t10:= e or t5; t11:= t9 and t10; g:= a xor t11; t13:= d or t1; t14:= t2 xor h; t15:= g xor t13; f:= t14 xor t15;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+3)]; f:= f xor l_key[4*(i+3)+1]; g:= g xor l_key[4*(i+3)+2]; h:= h xor l_key[4*(i+3)+3];
    t1:= e xor g; t2:= e or h; t3:= e and f; t4:= e and h; t5:= f or t4; t6:= t1 and t2; b:= t5 xor t6; t8:= f xor h; t9:= g or t3; t10:= t6 xor t8; d:= t9 xor t10; t12:= g xor t3; t13:= t2 and d; c:= t12 xor t13; t15:= not c; t16:= t2 xor t3; t17:= b and t15; a:= t16 xor t17;
    a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    a:= a xor l_key[4*(i+4)]; b:= b xor l_key[4*(i+4)+1]; c:= c xor l_key[4*(i+4)+2]; d:= d xor l_key[4*(i+4)+3];
    t1:= not a; t2:= a xor d; t3:= a xor b; t4:= c xor t1; t5:= t2 or t3; e:= t4 xor t5; t7:= not e; t8:= b or t7; h:= t2 xor t8; t10:= a and e; t11:= b xor h; t12:= t8 and t11; g:= t10 xor t12; t14:= a or t7; t15:= t3 xor t14; t16:= h and g; f:= t15 xor t16;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+5)]; f:= f xor l_key[4*(i+5)+1]; g:= g xor l_key[4*(i+5)+2]; h:= h xor l_key[4*(i+5)+3];
    t1:= not e; t2:= e xor f; t3:= e xor h; t4:= g xor t1; t5:= t2 or t3; a:= t4 xor t5; t7:= not h; t8:= a and t7; b:= t2 xor t8; t10:= f or b; t11:= g or a; t12:= t7 xor t10; d:= t11 xor t12; t14:= h or b; t15:= t1 xor t14; t16:= a or d; c:= t15 xor t16;
    a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    a:= a xor l_key[4*(i+6)]; b:= b xor l_key[4*(i+6)+1]; c:= c xor l_key[4*(i+6)+2]; d:= d xor l_key[4*(i+6)+3];
    t1:= a xor c; t2:= b or d; t3:= b xor c; t4:= not t3; t5:= a and d; f:= t4 xor t5; t7:= b or c; t8:= d xor t1; t9:= t7 and t8; h:= t2 xor t9; t11:= t1 and t7; t12:= t4 xor t8; t13:= h and t11; e:= t12 xor t13; t15:= t3 xor t11; t16:= h or t15; g:= t12 xor t16;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+7)]; f:= f xor l_key[4*(i+7)+1]; g:= g xor l_key[4*(i+7)+2]; h:= h xor l_key[4*(i+7)+3];
    t1:= not g; t2:= f xor g; t3:= f or t1; t4:= h xor t3; t5:= e and t4; d:= t2 xor t5; t7:= e xor h; t8:= f xor t5; t9:= t2 or t8; b:= t7 xor t9; t11:= h and t3; t12:= t5 xor b; t13:= d and t12; c:= t11 xor t13; t15:= t1 or t4; t16:= t12 xor c; a:= t15 xor t16;

    Inc(i,8);
    if i < 32 then
    begin
      a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    end;
  end;
  a:= a xor l_key[128]; b:= b xor l_key[128+1]; c:= c xor l_key[128+2]; d:= d xor l_key[128+3];

  PDWord(longword(@OutData)+ 0)^:= a;
  PDWord(longword(@OutData)+ 4)^:= b;
  PDWord(longword(@OutData)+ 8)^:= c;
  PDWord(longword(@OutData)+12)^:= d;
end;

procedure Tserpent.DecryptECB(const InData; var OutData);
var
  i: integer;
  a, b, c, d, e, f, g, h: dword;
  t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16: dword;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');

  a:= PDWord(@InData)^;
  b:= PDWord(longword(@InData)+4)^;
  c:= PDWord(longword(@InData)+8)^;
  d:= PDWord(longword(@InData)+12)^;

  i:= 32;
  a:= a xor l_key[4*32]; b:= b xor l_key[4*32+1]; c:= c xor l_key[4*32+2]; d:= d xor l_key[4*32+3]; 
  while i > 0 do
  begin
    if i < 32 then
    begin
      c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19); 
    end;

    t1:= a and b; t2:= a or b; t3:= c or t1; t4:= d and t2; h:= t3 xor t4; t6:= not d; t7:= b xor t4; t8:= h xor t6; t9:= t7 or t8; f:= a xor t9; t11:= c xor t7; t12:= d or f; e:= t11 xor t12; t14:= a and h; t15:= t3 xor f; t16:= e xor t14; g:= t15 xor t16; 
    e:= e xor l_key[4*(i-1)]; f:= f xor l_key[4*(i-1)+1]; g:= g xor l_key[4*(i-1)+2]; h:= h xor l_key[4*(i-1)+3]; 
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19); 
    t1:= not g; t2:= e xor g; t3:= f xor h; t4:= e or t1; b:= t3 xor t4; t6:= e or f; t7:= f and t2; t8:= b xor t6; t9:= t7 or t8; a:= g xor t9; t11:= not b; t12:= h or t2; t13:= t9 xor t11; d:= t12 xor t13; t15:= f xor t11; t16:= a and d; c:= t15 xor t16; 
    a:= a xor l_key[4*(i-2)]; b:= b xor l_key[4*(i-2)+1]; c:= c xor l_key[4*(i-2)+2]; d:= d xor l_key[4*(i-2)+3];
    c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19); 
    t1:= not c; t2:= b and t1; t3:= d xor t2; t4:= a and t3; t5:= b xor t1; h:= t4 xor t5; t7:= b or h; t8:= a and t7; f:= t3 xor t8; t10:= a or d; t11:= t1 xor t7; e:= t10 xor t11; t13:= a xor c; t14:= b and t10; t15:= t4 or t13; g:= t14 xor t15; 
    e:= e xor l_key[4*(i-3)]; f:= f xor l_key[4*(i-3)+1]; g:= g xor l_key[4*(i-3)+2]; h:= h xor l_key[4*(i-3)+3]; 
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19); 
    t1:= g xor h; t2:= g or h; t3:= f xor t2; t4:= e and t3; b:= t1 xor t4; t6:= e xor h; t7:= f or h; t8:= t6 and t7; d:= t3 xor t8; t10:= not e; t11:= g xor d; t12:= t10 or t11; a:= t3 xor t12; t14:= g or t4; t15:= t7 xor t14; t16:= d or t10; c:= t15 xor t16; 
    a:= a xor l_key[4*(i-4)]; b:= b xor l_key[4*(i-4)+1]; c:= c xor l_key[4*(i-4)+2]; d:= d xor l_key[4*(i-4)+3]; 
    c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19); 
    t1:= b xor c; t2:= b or c; t3:= a xor c; t4:= t2 xor t3; t5:= d or t4; e:= t1 xor t5; t7:= a xor d; t8:= t1 or t5; t9:= t2 xor t7; g:= t8 xor t9; t11:= a and t4; t12:= e or t9; f:= t11 xor t12; t14:= a and g; t15:= t2 xor t14; t16:= e and t15; h:= t4 xor t16; 
    e:= e xor l_key[4*(i-5)]; f:= f xor l_key[4*(i-5)+1]; g:= g xor l_key[4*(i-5)+2]; h:= h xor l_key[4*(i-5)+3]; 
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19); 
    t1:= f xor h; t2:= not t1; t3:= e xor g; t4:= g xor t1; t5:= f and t4; a:= t3 xor t5; t7:= e or t2; t8:= h xor t7; t9:= t3 or t8; d:= t1 xor t9; t11:= not t4; t12:= a or d; b:= t11 xor t12; t14:= h and t11; t15:= t3 xor t12; c:= t14 xor t15; 
    a:= a xor l_key[4*(i-6)]; b:= b xor l_key[4*(i-6)+1]; c:= c xor l_key[4*(i-6)+2]; d:= d xor l_key[4*(i-6)+3]; 
    c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19);
    t1:= a xor d; t2:= a and b; t3:= b xor c; t4:= a xor t3; t5:= b or d; h:= t4 xor t5; t7:= c or t1; t8:= b xor t7; t9:= t4 and t8; f:= t1 xor t9; t11:= not t2; t12:= h and f; t13:= t9 xor t11; g:= t12 xor t13; t15:= a and d; t16:= c xor t13; e:= t15 xor t16;
    e:= e xor l_key[4*(i-7)]; f:= f xor l_key[4*(i-7)+1]; g:= g xor l_key[4*(i-7)+2]; h:= h xor l_key[4*(i-7)+3];
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19);
    t1:= e xor h; t2:= g xor h; t3:= not t2; t4:= e or f; c:= t3 xor t4; t6:= f xor t1; t7:= g or t6; t8:= e xor t7; t9:= t2 and t8; b:= t6 xor t9; t11:= not t8; t12:= f and h; t13:= b or t12; d:= t11 xor t13; t15:= t2 xor t12; t16:= b or d; a:= t15 xor t16;
    a:= a xor l_key[4*(i-8)]; b:= b xor l_key[4*(i-8)+1]; c:= c xor l_key[4*(i-8)+2]; d:= d xor l_key[4*(i-8)+3];
    Dec(i,8);
  end;

  PDWord(longword(@OutData)+ 0)^:= a;
  PDWord(longword(@OutData)+ 4)^:= b;
  PDWord(longword(@OutData)+ 8)^:= c;
  PDWord(longword(@OutData)+12)^:= d;
end;

var
  MDS: array[0..3,0..255] of dword;
  MDSDone: boolean;


function LFSR1(x: DWord): DWord;
begin
  if (x and 1)<> 0 then
    Result:= (x shr 1) xor (MDS_GF_FDBK div 2)
  else
    Result:= (x shr 1);
end;
function LFSR2(x: DWord): DWord;
begin
  if (x and 2)<> 0 then
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2)
  else
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2);
end;
function Mul_X(x: DWord): DWord;
begin
  Result:= x xor LFSR2(x);
end;
function Mul_Y(x: DWord): DWord;
begin
  Result:= x xor LFSR1(x) xor LFSR2(x);
end;

function RS_MDS_Encode(lK0, lK1: DWord): DWord;
var
  lR, nJ, lG2, lG3: DWord;
  bB: byte;
begin
  lR:= lK1;
  for nJ:= 0 to 3 do
  begin
    bB:= lR shr 24;
    if (bB and $80)<> 0 then
      lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
    else
      lG2:= (bB shl 1) and $FF;
    if (bB and 1)<> 0 then
      lG3:= ((bB shr 1) and $7f) xor (RS_GF_FDBK shr 1) xor lG2
    else
      lG3:= ((bB shr 1) and $7f) xor lG2;
    lR:= (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor bB;
  end;
  lR:= lR xor lK0;
  for nJ:= 0 to 3 do
  begin
    bB:= lR shr 24;
    if (bB and $80)<> 0 then
      lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
    else
      lG2:= (bB shl 1) and $FF;
    if (bB and 1)<> 0 then
      lG3:= ((bB shr 1) and $7f) xor (RS_GF_FDBK shr 1) xor lG2
    else
      lG3:= ((bB shr 1) and $7f) xor lG2;
    lR:= (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor bB;
  end;
  Result:= lR;
end;

function f32(x: DWord; K32: PDWordArray; Len: DWord): DWord;
var
  t0, t1, t2, t3: DWord;
begin
  t0:= x and $FF;
  t1:= (x shr 8) and $FF;
  t2:= (x shr 16) and $FF;
  t3:= x shr 24;
  if Len= 256 then
  begin
    t0:= p8x8[1,t0] xor ((K32^[3]) and $FF);
    t1:= p8x8[0,t1] xor ((K32^[3] shr  8) and $FF);
    t2:= p8x8[0,t2] xor ((K32^[3] shr 16) and $FF);
    t3:= p8x8[1,t3] xor ((K32^[3] shr 24));
  end;
  if Len>= 192 then
  begin
    t0:= p8x8[1,t0] xor ((K32^[2]) and $FF);
    t1:= p8x8[1,t1] xor ((K32^[2] shr  8) and $FF);
    t2:= p8x8[0,t2] xor ((K32^[2] shr 16) and $FF);
    t3:= p8x8[0,t3] xor ((K32^[2] shr 24));
  end;
  Result:= MDS[0,p8x8[0,p8x8[0,t0] xor ((K32^[1]) and $FF)] xor ((K32^[0]) and $FF)] xor
           MDS[1,p8x8[0,p8x8[1,t1] xor ((K32^[1] shr  8) and $FF)] xor ((K32^[0] shr  8) and $FF)] xor
           MDS[2,p8x8[1,p8x8[0,t2] xor ((K32^[1] shr 16) and $FF)] xor ((K32^[0] shr 16) and $FF)] xor
           MDS[3,p8x8[1,p8x8[1,t3] xor ((K32^[1] shr 24))] xor ((K32^[0] shr 24))];
end;

procedure Xor256(Dst, Src: PDWordArray; v: byte);
var
  i, j: DWord;
begin
  i:= 0;
  j:= v * $01010101;
  while i< 64 do
  begin
    Dst^[i]:= Src^[i] xor j;
    Dst^[i+1]:= Src^[i+1] xor j;
    Dst^[i+2]:= Src^[i+2] xor j;
    Dst^[i+3]:= Src^[i+3] xor j;
    Inc(i,4);
  end;
end;


procedure PreCompMDS;
var
  m1, mx, my: array[0..1] of DWord;
  nI: longword;
begin
  for nI:= 0 to 255 do
  begin
    m1[0]:= p8x8[0,nI];
    mx[0]:= Mul_X(m1[0]);
    my[0]:= Mul_Y(m1[0]);
    m1[1]:= p8x8[1,nI];
    mx[1]:= Mul_X(m1[1]);
    my[1]:= Mul_Y(m1[1]);
    mds[0,nI]:= (m1[1] shl 0) or
                (mx[1] shl 8) or
                (my[1] shl 16) or
                (my[1] shl 24);
    mds[1,nI]:= (my[0] shl 0) or
                (my[0] shl 8) or
                (mx[0] shl 16) or
                (m1[0] shl 24);
    mds[2,nI]:= (mx[1] shl 0) or
                (my[1] shl 8) or
                (m1[1] shl 16) or
                (my[1] shl 24);
    mds[3,nI]:= (mx[0] shl 0) or
                (m1[0] shl 8) or
                (my[0] shl 16) or
                (mx[0] shl 24);
  end;
end;
{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}
destructor TTwofish.Destroy;
begin
// All Strings := '';
// Free_And_Nil(All PObj);

 inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}
function NewTwofish;
begin
New(Result, Create);
  if not MDSDone then
  begin
    PreCompMDS;
    MDSDone:= true;
  end;
// code
end;
////////////////////////////////////////////////////////////////////////////////


procedure Ttwofish.InitKey(const Key; Size: longword);
const
  subkeyCnt= ROUNDSUBKEYS + 2*NUMROUNDSTF;
var
  key32: array[0..7] of DWord;
  k32e, k32o, sboxkeys: array[0..3] of DWord;
  k64Cnt, i, j, A, B, q: DWord;
  L0, L1: array[0..255] of byte;
begin
burn;
  FillChar(Key32,Sizeof(Key32),0);
  Move(Key,Key32,Size div 8);
  if Size<= 128 then           { pad the key to either 128bit, 192bit or 256bit}
    Size:= 128
  else if Size<= 192 then
    Size:= 192
  else
    Size:= 256;
  k64Cnt:= Size div 64;
  j:= k64Cnt-1;
  for i:= 0 to j do
  begin
    k32e[i]:= key32[2*i];
    k32o[i]:= key32[2*i+1];
    sboxKeys[j]:= RS_MDS_Encode(k32e[i],k32o[i]);
    Dec(j);
  end;
  q:= 0;
  for i:= 0 to ((subkeyCnt div 2)-1) do
  begin
    A:= f32(q,@k32e,Size);
    B:= f32(q+SK_BUMP,@k32o,Size);
    B:= (B shl 8) or (B shr 24);
    SubKeys[2*i]:= A+B;
    B:= A + 2*B;
    SubKeys[2*i+1]:= (B shl SK_ROTL) or (B shr (32 - SK_ROTL)); 
    Inc(q,SK_STEP);
  end;
  case Size of
    128: begin
           Xor256(@L0,@p8x8[0],(sboxKeys[1] and $FF));
           A:= (sboxKeys[0] and $FF);
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,L0[i]] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[1],(sboxKeys[1] shr 8) and $FF);
           A:= (sboxKeys[0] shr 8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,L0[i]] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[0],(sboxKeys[1] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,L0[i]] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[1],(sboxKeys[1] shr 24));
           A:= (sboxKeys[0] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,L0[i]] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,L0[i+1]] xor A];
             Inc(i,2);
           end;
         end;
    192: begin
           Xor256(@L0,@p8x8[1],sboxKeys[2] and $FF);
           A:= sboxKeys[0] and $FF;
           B:= sboxKeys[1] and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,p8x8[0,L0[i]] xor B] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[1],(sboxKeys[2] shr 8) and $FF);
           A:= (sboxKeys[0] shr 8) and $FF;
           B:= (sboxKeys[1] shr 8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,p8x8[1,L0[i]] xor B] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[0],(sboxKeys[2] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           B:= (sboxKeys[1] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,p8x8[0,L0[i]] xor B] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[0],(sboxKeys[2] shr 24));
           A:= (sboxKeys[0] shr 24);
           B:= (sboxKeys[1] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,p8x8[1,L0[i]] xor B] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
         end;
    256: begin
           Xor256(@L1,@p8x8[1],(sboxKeys[3]) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[1,L1[i]];
             L0[i+1]:= p8x8[1,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2]) and $FF);
           A:= (sboxKeys[0]) and $FF;
           B:= (sboxKeys[1]) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,p8x8[0,L0[i]] xor B] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L1,@p8x8[0],(sboxKeys[3] shr  8) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[1,L1[i]];
             L0[i+1]:= p8x8[1,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2] shr  8) and $FF);
           A:= (sboxKeys[0] shr  8) and $FF;
           B:= (sboxKeys[1] shr  8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,p8x8[1,L0[i]] xor B] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;

           Xor256(@L1,@p8x8[0],(sboxKeys[3] shr 16) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[0,L1[i]];
             L0[i+1]:= p8x8[0,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           B:= (sboxKeys[1] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,p8x8[0,L0[i]] xor B] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L1,@p8x8[1],(sboxKeys[3] shr 24));
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[0,L1[i]];
             L0[i+1]:= p8x8[0,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2] shr 24));
           A:= (sboxKeys[0] shr 24);
           B:= (sboxKeys[1] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,p8x8[1,L0[i]] xor B] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
         end;
  end;
end;

procedure Ttwofish.Burn;
begin
  FillChar(sBox,Sizeof(sBox),$FF);
  FillChar(SubKeys,Sizeof(SubKeys),$FF);
  inherited Burn;
end;

procedure Ttwofish.EncryptECB(const InData; var OutData);
var
  i: longword;
  t0, t1: DWord;
  X: array[0..3] of DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  x[0]:= PDWord(@InData)^ xor SubKeys[INPUTWHITEN];
  x[1]:= PDWord(longword(@InData)+4)^ xor SubKeys[INPUTWHITEN+1];
  x[2]:= PDWord(longword(@InData)+8)^ xor SubKeys[INPUTWHITEN+2];
  x[3]:= PDWord(longword(@InData)+12)^ xor SubKeys[INPUTWHITEN+3];
  i:= 0;
  while i<= NUMROUNDSTF-2 do
  begin
    t0:= sBox[0,(x[0] shl 1) and $1fe] xor sBox[0,((x[0] shr 7) and $1fe)+1]
      xor sBox[2,(x[0] shr 15) and $1fe] xor sBox[2,((x[0] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[1] shr 23) and $1fe)] xor sBox[0,((x[1] shl 1) and $1fe)+1]
      xor sBox[2,((x[1] shr 7) and $1fe)] xor sBox[2,((x[1] shr 15) and $1fe)+1];
    x[3]:= (x[3] shl 1) or (x[3] shr 31);
    x[2]:= x[2] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*i+1]);
    x[2]:= (x[2] shr 1) or (x[2] shl 31);

    t0:= sBox[0,(x[2] shl 1) and $1fe] xor sBox[0,((x[2] shr 7) and $1fe)+1]
      xor sBox[2,((x[2] shr 15) and $1fe)] xor sBox[2,((x[2] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[3] shr 23) and $1fe)] xor sBox[0,((x[3] shl 1) and $1fe)+1]
      xor sBox[2,((x[3] shr 7) and $1fe)] xor sBox[2,((x[3] shr 15) and $1fe)+1];
    x[1]:= (x[1] shl 1) or (x[1] shr 31);
    x[0]:= x[0] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)+1]);
    x[0]:= (x[0] shr 1) or (x[0] shl 31);
    Inc(i,2);
  end;
  PDWord(longword(@OutData)+ 0)^:= x[2] xor SubKeys[OUTPUTWHITEN];
  PDWord(longword(@OutData)+ 4)^:= x[3] xor SubKeys[OUTPUTWHITEN+1];
  PDWord(longword(@OutData)+ 8)^:= x[0] xor SubKeys[OUTPUTWHITEN+2];
  PDWord(longword(@OutData)+12)^:= x[1] xor SubKeys[OUTPUTWHITEN+3];
end;

procedure Ttwofish.DecryptECB(const InData; var OutData);
var
  i: integer;
  t0, t1: DWord;
  X: array[0..3] of DWord;
begin
//  if not fInitialized then
//    raise EDCP_blockcipher.Create('Cipher not initialized');
  X[2]:= PDWord(@InData)^ xor SubKeys[OUTPUTWHITEN];
  X[3]:= PDWord(longword(@InData)+4)^ xor SubKeys[OUTPUTWHITEN+1];
  X[0]:= PDWord(longword(@InData)+8)^ xor SubKeys[OUTPUTWHITEN+2];
  X[1]:= PDWord(longword(@InData)+12)^ xor SubKeys[OUTPUTWHITEN+3];
  i:= NUMROUNDSTF-2;
  while i>= 0 do
  begin
    t0:= sBox[0,(x[2] shl 1) and $1fe] xor sBox[0,((x[2] shr 7) and $1fe)+1]
      xor sBox[2,((x[2] shr 15) and $1fe)] xor sBox[2,((x[2] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[3] shr 23) and $1fe)] xor sBox[0,((x[3] shl 1) and $1fe)+1]
      xor sBox[2,((x[3] shr 7) and $1fe)] xor sBox[2,((x[3] shr 15) and $1fe)+1];
    x[0]:= (x[0] shl 1) or (x[0] shr 31);
    x[0]:= x[0] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)+1]);
    x[1]:= (x[1] shr 1) or (x[1] shl 31);

    t0:= sBox[0,(x[0] shl 1) and $1fe] xor sBox[0,((x[0] shr 7) and $1fe)+1]
      xor sBox[2,(x[0] shr 15) and $1fe] xor sBox[2,((x[0] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[1] shr 23) and $1fe)] xor sBox[0,((x[1] shl 1) and $1fe)+1]
      xor sBox[2,((x[1] shr 7) and $1fe)] xor sBox[2,((x[1] shr 15) and $1fe)+1];
    x[2]:= (x[2] shl 1) or (x[2] shr 31);
    x[2]:= x[2] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*i+1]);
    x[3]:= (x[3] shr 1) or (x[3] shl 31);
    Dec(i,2);
  end;
  PDWord(longword(@OutData)+ 0)^:= X[0] xor SubKeys[INPUTWHITEN];
  PDWord(longword(@OutData)+ 4)^:= X[1] xor SubKeys[INPUTWHITEN+1];
  PDWord(longword(@OutData)+ 8)^:= X[2] xor SubKeys[INPUTWHITEN+2];
  PDWord(longword(@OutData)+12)^:= X[3] xor SubKeys[INPUTWHITEN+3];
end;




initialization
  ice_sboxdone:= false;
  MDSdone:= false;

end.
