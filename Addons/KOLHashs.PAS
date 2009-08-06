unit KOLHashs;

interface

uses Windows, KOL;

type
  PHAVAL = ^THAVAL;
  THAVAL = object(TObj)
  protected
    // code
    LenHi, LenLo: longword;
      Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..127] of byte;
    fInitialized: Boolean;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLHAVAL = PHAVAL;

  PMD4 = ^TMD4;
  TMD4 = object(TObj)
  protected
    // code
    LenHi, LenLo: longword;
      Index: DWord;
    CurrentHash: array[0..3] of DWord;
    HashBuffer: array[0..63] of byte;
    fInitialized: Boolean;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLMD4 = PMD4;

  PMD5 = ^TMD5;
  TMD5 = object(TObj)
  protected
    // code
    LenHi, LenLo: longword;
      Index: DWord;
    CurrentHash: array[0..3] of DWord;
    HashBuffer: array[0..63] of byte;
    fInitialized: Boolean;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLMD5 = PMD5;

  PRMD128 = ^TRMD128;
  TRMD128 = object(TObj)
  protected
    // code
    fInitialized: Boolean;
    LenHi, LenLo: longword;
      Index: DWord;
    CurrentHash: array[0..3] of DWord;
    HashBuffer: array[0..63] of byte;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLRMD128 = PRMD128;

  PRMD160 = ^TRMD160;
  TRMD160 = object(TObj)
  protected
    // code
    fInitialized: Boolean;
    LenHi, LenLo: longword;
      Index: DWord;
    CurrentHash: array[0..4] of DWord;
    HashBuffer: array[0..63] of byte;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLRMD160 = PRMD160;

  PSHA1 = ^TSHA1;
  TSHA1 = object(TObj)
  protected
    // code
    fInitialized: Boolean;
    LenHi, LenLo: longword;
      Index: DWord;
    CurrentHash: array[0..4] of DWord;
    HashBuffer: array[0..63] of byte;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLSHA1 = PSHA1;

  PSHA256 = ^TSHA256;
  TSHA256 = object(TObj)
  protected
    // code
    fInitialized: Boolean;
    LenHi, LenLo: longword;
      Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..63] of byte;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLSHA256 = PSHA256;

  PSHA384 = ^TSHA384;
  TSHA384 = object(TObj)
  protected
    fInitialized: Boolean;
    LenHi, LenLo: int64;
      Index: DWord;
    CurrentHash: array[0..7] of int64;
    HashBuffer: array[0..127] of byte;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLSHA384 = PSHA384;

  PSHA512 = ^TSHA512;
  TSHA512 = object(TObj)
  protected
    // code
    fInitialized: Boolean;
    LenHi, LenLo: Int64;
      Index: DWORD;
    CurrentHash: array[0..7] of Int64;
    HashBuffer: array[0..127] of Byte;
    procedure Compress;
  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: DWORD);
    procedure UpdateStr(const s: string);
    procedure Final(var Digest);
    destructor Destroy; virtual;
  end;

  TKOLSHA512 = PSHA512;

const
{$WARNINGS OFF}
  tiger1            : array[0..255] of int64 = (
    $02AAB17CF7E90C5E, $AC424B03E243A8EC,
    $72CD5BE30DD5FCD3, $6D019B93F6F97F3A,
    $CD9978FFD21F9193, $7573A1C9708029E2,
    $B164326B922A83C3, $46883EEE04915870,
    $EAACE3057103ECE6, $C54169B808A3535C,
    $4CE754918DDEC47C, $0AA2F4DFDC0DF40C,
    $10B76F18A74DBEFA, $C6CCB6235AD1AB6A,
    $13726121572FE2FF, $1A488C6F199D921E,
    $4BC9F9F4DA0007CA, $26F5E6F6E85241C7,
    $859079DBEA5947B6, $4F1885C5C99E8C92,
    $D78E761EA96F864B, $8E36428C52B5C17D,
    $69CF6827373063C1, $B607C93D9BB4C56E,
    $7D820E760E76B5EA, $645C9CC6F07FDC42,
    $BF38A078243342E0, $5F6B343C9D2E7D04,
    $F2C28AEB600B0EC6, $6C0ED85F7254BCAC,
    $71592281A4DB4FE5, $1967FA69CE0FED9F,
    $FD5293F8B96545DB, $C879E9D7F2A7600B,
    $860248920193194E, $A4F9533B2D9CC0B3,
    $9053836C15957613, $DB6DCF8AFC357BF1,
    $18BEEA7A7A370F57, $037117CA50B99066,
    $6AB30A9774424A35, $F4E92F02E325249B,
    $7739DB07061CCAE1, $D8F3B49CECA42A05,
    $BD56BE3F51382F73, $45FAED5843B0BB28,
    $1C813D5C11BF1F83, $8AF0E4B6D75FA169,
    $33EE18A487AD9999, $3C26E8EAB1C94410,
    $B510102BC0A822F9, $141EEF310CE6123B,
    $FC65B90059DDB154, $E0158640C5E0E607,
    $884E079826C3A3CF, $930D0D9523C535FD,
    $35638D754E9A2B00, $4085FCCF40469DD5,
    $C4B17AD28BE23A4C, $CAB2F0FC6A3E6A2E,
    $2860971A6B943FCD, $3DDE6EE212E30446,
    $6222F32AE01765AE, $5D550BB5478308FE,
    $A9EFA98DA0EDA22A, $C351A71686C40DA7,
    $1105586D9C867C84, $DCFFEE85FDA22853,
    $CCFBD0262C5EEF76, $BAF294CB8990D201,
    $E69464F52AFAD975, $94B013AFDF133E14,
    $06A7D1A32823C958, $6F95FE5130F61119,
    $D92AB34E462C06C0, $ED7BDE33887C71D2,
    $79746D6E6518393E, $5BA419385D713329,
    $7C1BA6B948A97564, $31987C197BFDAC67,
    $DE6C23C44B053D02, $581C49FED002D64D,
    $DD474D6338261571, $AA4546C3E473D062,
    $928FCE349455F860, $48161BBACAAB94D9,
    $63912430770E6F68, $6EC8A5E602C6641C,
    $87282515337DDD2B, $2CDA6B42034B701B,
    $B03D37C181CB096D, $E108438266C71C6F,
    $2B3180C7EB51B255, $DF92B82F96C08BBC,
    $5C68C8C0A632F3BA, $5504CC861C3D0556,
    $ABBFA4E55FB26B8F, $41848B0AB3BACEB4,
    $B334A273AA445D32, $BCA696F0A85AD881,
    $24F6EC65B528D56C, $0CE1512E90F4524A,
    $4E9DD79D5506D35A, $258905FAC6CE9779,
    $2019295B3E109B33, $F8A9478B73A054CC,
    $2924F2F934417EB0, $3993357D536D1BC4,
    $38A81AC21DB6FF8B, $47C4FBF17D6016BF,
    $1E0FAADD7667E3F5, $7ABCFF62938BEB96,
    $A78DAD948FC179C9, $8F1F98B72911E50D,
    $61E48EAE27121A91, $4D62F7AD31859808,
    $ECEBA345EF5CEAEB, $F5CEB25EBC9684CE,
    $F633E20CB7F76221, $A32CDF06AB8293E4,
    $985A202CA5EE2CA4, $CF0B8447CC8A8FB1,
    $9F765244979859A3, $A8D516B1A1240017,
    $0BD7BA3EBB5DC726, $E54BCA55B86ADB39,
    $1D7A3AFD6C478063, $519EC608E7669EDD,
    $0E5715A2D149AA23, $177D4571848FF194,
    $EEB55F3241014C22, $0F5E5CA13A6E2EC2,
    $8029927B75F5C361, $AD139FABC3D6E436,
    $0D5DF1A94CCF402F, $3E8BD948BEA5DFC8,
    $A5A0D357BD3FF77E, $A2D12E251F74F645,
    $66FD9E525E81A082, $2E0C90CE7F687A49,
    $C2E8BCBEBA973BC5, $000001BCE509745F,
    $423777BBE6DAB3D6, $D1661C7EAEF06EB5,
    $A1781F354DAACFD8, $2D11284A2B16AFFC,
    $F1FC4F67FA891D1F, $73ECC25DCB920ADA,
    $AE610C22C2A12651, $96E0A810D356B78A,
    $5A9A381F2FE7870F, $D5AD62EDE94E5530,
    $D225E5E8368D1427, $65977B70C7AF4631,
    $99F889B2DE39D74F, $233F30BF54E1D143,
    $9A9675D3D9A63C97, $5470554FF334F9A8,
    $166ACB744A4F5688, $70C74CAAB2E4AEAD,
    $F0D091646F294D12, $57B82A89684031D1,
    $EFD95A5A61BE0B6B, $2FBD12E969F2F29A,
    $9BD37013FEFF9FE8, $3F9B0404D6085A06,
    $4940C1F3166CFE15, $09542C4DCDF3DEFB,
    $B4C5218385CD5CE3, $C935B7DC4462A641,
    $3417F8A68ED3B63F, $B80959295B215B40,
    $F99CDAEF3B8C8572, $018C0614F8FCB95D,
    $1B14ACCD1A3ACDF3, $84D471F200BB732D,
    $C1A3110E95E8DA16, $430A7220BF1A82B8,
    $B77E090D39DF210E, $5EF4BD9F3CD05E9D,
    $9D4FF6DA7E57A444, $DA1D60E183D4A5F8,
    $B287C38417998E47, $FE3EDC121BB31886,
    $C7FE3CCC980CCBEF, $E46FB590189BFD03,
    $3732FD469A4C57DC, $7EF700A07CF1AD65,
    $59C64468A31D8859, $762FB0B4D45B61F6,
    $155BAED099047718, $68755E4C3D50BAA6,
    $E9214E7F22D8B4DF, $2ADDBF532EAC95F4,
    $32AE3909B4BD0109, $834DF537B08E3450,
    $FA209DA84220728D, $9E691D9B9EFE23F7,
    $0446D288C4AE8D7F, $7B4CC524E169785B,
    $21D87F0135CA1385, $CEBB400F137B8AA5,
    $272E2B66580796BE, $3612264125C2B0DE,
    $057702BDAD1EFBB2, $D4BABB8EACF84BE9,
    $91583139641BC67B, $8BDC2DE08036E024,
    $603C8156F49F68ED, $F7D236F7DBEF5111,
    $9727C4598AD21E80, $A08A0896670A5FD7,
    $CB4A8F4309EBA9CB, $81AF564B0F7036A1,
    $C0B99AA778199ABD, $959F1EC83FC8E952,
    $8C505077794A81B9, $3ACAAF8F056338F0,
    $07B43F50627A6778, $4A44AB49F5ECCC77,
    $3BC3D6E4B679EE98, $9CC0D4D1CF14108C,
    $4406C00B206BC8A0, $82A18854C8D72D89,
    $67E366B35C3C432C, $B923DD61102B37F2,
    $56AB2779D884271D, $BE83E1B0FF1525AF,
    $FB7C65D4217E49A9, $6BDBE0E76D48E7D4,
    $08DF828745D9179E, $22EA6A9ADD53BD34,
    $E36E141C5622200A, $7F805D1B8CB750EE,
    $AFE5C7A59F58E837, $E27F996A4FB1C23C,
    $D3867DFB0775F0D0, $D0E673DE6E88891A,
    $123AEB9EAFB86C25, $30F1D5D5C145B895,
    $BB434A2DEE7269E7, $78CB67ECF931FA38,
    $F33B0372323BBF9C, $52D66336FB279C74,
    $505F33AC0AFB4EAA, $E8A5CD99A2CCE187,
    $534974801E2D30BB, $8D2D5711D5876D90,
    $1F1A412891BC038E, $D6E2E71D82E56648,
    $74036C3A497732B7, $89B67ED96361F5AB,
    $FFED95D8F1EA02A2, $E72B3BD61464D43D,
    $A6300F170BDC4820, $EBC18760ED78A77A);

  tiger2            : array[0..255] of int64 = (
    $E6A6BE5A05A12138, $B5A122A5B4F87C98,
    $563C6089140B6990, $4C46CB2E391F5DD5,
    $D932ADDBC9B79434, $08EA70E42015AFF5,
    $D765A6673E478CF1, $C4FB757EAB278D99,
    $DF11C6862D6E0692, $DDEB84F10D7F3B16,
    $6F2EF604A665EA04, $4A8E0F0FF0E0DFB3,
    $A5EDEEF83DBCBA51, $FC4F0A2A0EA4371E,
    $E83E1DA85CB38429, $DC8FF882BA1B1CE2,
    $CD45505E8353E80D, $18D19A00D4DB0717,
    $34A0CFEDA5F38101, $0BE77E518887CAF2,
    $1E341438B3C45136, $E05797F49089CCF9,
    $FFD23F9DF2591D14, $543DDA228595C5CD,
    $661F81FD99052A33, $8736E641DB0F7B76,
    $15227725418E5307, $E25F7F46162EB2FA,
    $48A8B2126C13D9FE, $AFDC541792E76EEA,
    $03D912BFC6D1898F, $31B1AAFA1B83F51B,
    $F1AC2796E42AB7D9, $40A3A7D7FCD2EBAC,
    $1056136D0AFBBCC5, $7889E1DD9A6D0C85,
    $D33525782A7974AA, $A7E25D09078AC09B,
    $BD4138B3EAC6EDD0, $920ABFBE71EB9E70,
    $A2A5D0F54FC2625C, $C054E36B0B1290A3,
    $F6DD59FF62FE932B, $3537354511A8AC7D,
    $CA845E9172FADCD4, $84F82B60329D20DC,
    $79C62CE1CD672F18, $8B09A2ADD124642C,
    $D0C1E96A19D9E726, $5A786A9B4BA9500C,
    $0E020336634C43F3, $C17B474AEB66D822,
    $6A731AE3EC9BAAC2, $8226667AE0840258,
    $67D4567691CAECA5, $1D94155C4875ADB5,
    $6D00FD985B813FDF, $51286EFCB774CD06,
    $5E8834471FA744AF, $F72CA0AEE761AE2E,
    $BE40E4CDAEE8E09A, $E9970BBB5118F665,
    $726E4BEB33DF1964, $703B000729199762,
    $4631D816F5EF30A7, $B880B5B51504A6BE,
    $641793C37ED84B6C, $7B21ED77F6E97D96,
    $776306312EF96B73, $AE528948E86FF3F4,
    $53DBD7F286A3F8F8, $16CADCE74CFC1063,
    $005C19BDFA52C6DD, $68868F5D64D46AD3,
    $3A9D512CCF1E186A, $367E62C2385660AE,
    $E359E7EA77DCB1D7, $526C0773749ABE6E,
    $735AE5F9D09F734B, $493FC7CC8A558BA8,
    $B0B9C1533041AB45, $321958BA470A59BD,
    $852DB00B5F46C393, $91209B2BD336B0E5,
    $6E604F7D659EF19F, $B99A8AE2782CCB24,
    $CCF52AB6C814C4C7, $4727D9AFBE11727B,
    $7E950D0C0121B34D, $756F435670AD471F,
    $F5ADD442615A6849, $4E87E09980B9957A,
    $2ACFA1DF50AEE355, $D898263AFD2FD556,
    $C8F4924DD80C8FD6, $CF99CA3D754A173A,
    $FE477BACAF91BF3C, $ED5371F6D690C12D,
    $831A5C285E687094, $C5D3C90A3708A0A4,
    $0F7F903717D06580, $19F9BB13B8FDF27F,
    $B1BD6F1B4D502843, $1C761BA38FFF4012,
    $0D1530C4E2E21F3B, $8943CE69A7372C8A,
    $E5184E11FEB5CE66, $618BDB80BD736621,
    $7D29BAD68B574D0B, $81BB613E25E6FE5B,
    $071C9C10BC07913F, $C7BEEB7909AC2D97,
    $C3E58D353BC5D757, $EB017892F38F61E8,
    $D4EFFB9C9B1CC21A, $99727D26F494F7AB,
    $A3E063A2956B3E03, $9D4A8B9A4AA09C30,
    $3F6AB7D500090FB4, $9CC0F2A057268AC0,
    $3DEE9D2DEDBF42D1, $330F49C87960A972,
    $C6B2720287421B41, $0AC59EC07C00369C,
    $EF4EAC49CB353425, $F450244EEF0129D8,
    $8ACC46E5CAF4DEB6, $2FFEAB63989263F7,
    $8F7CB9FE5D7A4578, $5BD8F7644E634635,
    $427A7315BF2DC900, $17D0C4AA2125261C,
    $3992486C93518E50, $B4CBFEE0A2D7D4C3,
    $7C75D6202C5DDD8D, $DBC295D8E35B6C61,
    $60B369D302032B19, $CE42685FDCE44132,
    $06F3DDB9DDF65610, $8EA4D21DB5E148F0,
    $20B0FCE62FCD496F, $2C1B912358B0EE31,
    $B28317B818F5A308, $A89C1E189CA6D2CF,
    $0C6B18576AAADBC8, $B65DEAA91299FAE3,
    $FB2B794B7F1027E7, $04E4317F443B5BEB,
    $4B852D325939D0A6, $D5AE6BEEFB207FFC,
    $309682B281C7D374, $BAE309A194C3B475,
    $8CC3F97B13B49F05, $98A9422FF8293967,
    $244B16B01076FF7C, $F8BF571C663D67EE,
    $1F0D6758EEE30DA1, $C9B611D97ADEB9B7,
    $B7AFD5887B6C57A2, $6290AE846B984FE1,
    $94DF4CDEACC1A5FD, $058A5BD1C5483AFF,
    $63166CC142BA3C37, $8DB8526EB2F76F40,
    $E10880036F0D6D4E, $9E0523C9971D311D,
    $45EC2824CC7CD691, $575B8359E62382C9,
    $FA9E400DC4889995, $D1823ECB45721568,
    $DAFD983B8206082F, $AA7D29082386A8CB,
    $269FCD4403B87588, $1B91F5F728BDD1E0,
    $E4669F39040201F6, $7A1D7C218CF04ADE,
    $65623C29D79CE5CE, $2368449096C00BB1,
    $AB9BF1879DA503BA, $BC23ECB1A458058E,
    $9A58DF01BB401ECC, $A070E868A85F143D,
    $4FF188307DF2239E, $14D565B41A641183,
    $EE13337452701602, $950E3DCF3F285E09,
    $59930254B9C80953, $3BF299408930DA6D,
    $A955943F53691387, $A15EDECAA9CB8784,
    $29142127352BE9A0, $76F0371FFF4E7AFB,
    $0239F450274F2228, $BB073AF01D5E868B,
    $BFC80571C10E96C1, $D267088568222E23,
    $9671A3D48E80B5B0, $55B5D38AE193BB81,
    $693AE2D0A18B04B8, $5C48B4ECADD5335F,
    $FD743B194916A1CA, $2577018134BE98C4,
    $E77987E83C54A4AD, $28E11014DA33E1B9,
    $270CC59E226AA213, $71495F756D1A5F60,
    $9BE853FB60AFEF77, $ADC786A7F7443DBF,
    $0904456173B29A82, $58BC7A66C232BD5E,
    $F306558C673AC8B2, $41F639C6B6C9772A,
    $216DEFE99FDA35DA, $11640CC71C7BE615,
    $93C43694565C5527, $EA038E6246777839,
    $F9ABF3CE5A3E2469, $741E768D0FD312D2,
    $0144B883CED652C6, $C20B5A5BA33F8552,
    $1AE69633C3435A9D, $97A28CA4088CFDEC,
    $8824A43C1E96F420, $37612FA66EEEA746,
    $6B4CB165F9CF0E5A, $43AA1C06A0ABFB4A,
    $7F4DC26FF162796B, $6CBACC8E54ED9B0F,
    $A6B7FFEFD2BB253E, $2E25BC95B0A29D4F,
    $86D6A58BDEF1388C, $DED74AC576B6F054,
    $8030BDBC2B45805D, $3C81AF70E94D9289,
    $3EFF6DDA9E3100DB, $B38DC39FDFCC8847,
    $123885528D17B87E, $F2DA0ED240B1B642,
    $44CEFADCD54BF9A9, $1312200E433C7EE6,
    $9FFCC84F3A78C748, $F0CD1F72248576BB,
    $EC6974053638CFE4, $2BA7B67C0CEC4E4C,
    $AC2F4DF3E5CE32ED, $CB33D14326EA4C11,
    $A4E9044CC77E58BC, $5F513293D934FCEF,
    $5DC9645506E55444, $50DE418F317DE40A,
    $388CB31A69DDE259, $2DB4A83455820A86,
    $9010A91E84711AE9, $4DF7F0B7B1498371,
    $D62A2EABC0977179, $22FAC097AA8D5C0E);

  tiger3            : array[0..255] of int64 = (
    $F49FCC2FF1DAF39B, $487FD5C66FF29281,
    $E8A30667FCDCA83F, $2C9B4BE3D2FCCE63,
    $DA3FF74B93FBBBC2, $2FA165D2FE70BA66,
    $A103E279970E93D4, $BECDEC77B0E45E71,
    $CFB41E723985E497, $B70AAA025EF75017,
    $D42309F03840B8E0, $8EFC1AD035898579,
    $96C6920BE2B2ABC5, $66AF4163375A9172,
    $2174ABDCCA7127FB, $B33CCEA64A72FF41,
    $F04A4933083066A5, $8D970ACDD7289AF5,
    $8F96E8E031C8C25E, $F3FEC02276875D47,
    $EC7BF310056190DD, $F5ADB0AEBB0F1491,
    $9B50F8850FD58892, $4975488358B74DE8,
    $A3354FF691531C61, $0702BBE481D2C6EE,
    $89FB24057DEDED98, $AC3075138596E902,
    $1D2D3580172772ED, $EB738FC28E6BC30D,
    $5854EF8F63044326, $9E5C52325ADD3BBE,
    $90AA53CF325C4623, $C1D24D51349DD067,
    $2051CFEEA69EA624, $13220F0A862E7E4F,
    $CE39399404E04864, $D9C42CA47086FCB7,
    $685AD2238A03E7CC, $066484B2AB2FF1DB,
    $FE9D5D70EFBF79EC, $5B13B9DD9C481854,
    $15F0D475ED1509AD, $0BEBCD060EC79851,
    $D58C6791183AB7F8, $D1187C5052F3EEE4,
    $C95D1192E54E82FF, $86EEA14CB9AC6CA2,
    $3485BEB153677D5D, $DD191D781F8C492A,
    $F60866BAA784EBF9, $518F643BA2D08C74,
    $8852E956E1087C22, $A768CB8DC410AE8D,
    $38047726BFEC8E1A, $A67738B4CD3B45AA,
    $AD16691CEC0DDE19, $C6D4319380462E07,
    $C5A5876D0BA61938, $16B9FA1FA58FD840,
    $188AB1173CA74F18, $ABDA2F98C99C021F,
    $3E0580AB134AE816, $5F3B05B773645ABB,
    $2501A2BE5575F2F6, $1B2F74004E7E8BA9,
    $1CD7580371E8D953, $7F6ED89562764E30,
    $B15926FF596F003D, $9F65293DA8C5D6B9,
    $6ECEF04DD690F84C, $4782275FFF33AF88,
    $E41433083F820801, $FD0DFE409A1AF9B5,
    $4325A3342CDB396B, $8AE77E62B301B252,
    $C36F9E9F6655615A, $85455A2D92D32C09,
    $F2C7DEA949477485, $63CFB4C133A39EBA,
    $83B040CC6EBC5462, $3B9454C8FDB326B0,
    $56F56A9E87FFD78C, $2DC2940D99F42BC6,
    $98F7DF096B096E2D, $19A6E01E3AD852BF,
    $42A99CCBDBD4B40B, $A59998AF45E9C559,
    $366295E807D93186, $6B48181BFAA1F773,
    $1FEC57E2157A0A1D, $4667446AF6201AD5,
    $E615EBCACFB0F075, $B8F31F4F68290778,
    $22713ED6CE22D11E, $3057C1A72EC3C93B,
    $CB46ACC37C3F1F2F, $DBB893FD02AAF50E,
    $331FD92E600B9FCF, $A498F96148EA3AD6,
    $A8D8426E8B6A83EA, $A089B274B7735CDC,
    $87F6B3731E524A11, $118808E5CBC96749,
    $9906E4C7B19BD394, $AFED7F7E9B24A20C,
    $6509EADEEB3644A7, $6C1EF1D3E8EF0EDE,
    $B9C97D43E9798FB4, $A2F2D784740C28A3,
    $7B8496476197566F, $7A5BE3E6B65F069D,
    $F96330ED78BE6F10, $EEE60DE77A076A15,
    $2B4BEE4AA08B9BD0, $6A56A63EC7B8894E,
    $02121359BA34FEF4, $4CBF99F8283703FC,
    $398071350CAF30C8, $D0A77A89F017687A,
    $F1C1A9EB9E423569, $8C7976282DEE8199,
    $5D1737A5DD1F7ABD, $4F53433C09A9FA80,
    $FA8B0C53DF7CA1D9, $3FD9DCBC886CCB77,
    $C040917CA91B4720, $7DD00142F9D1DCDF,
    $8476FC1D4F387B58, $23F8E7C5F3316503,
    $032A2244E7E37339, $5C87A5D750F5A74B,
    $082B4CC43698992E, $DF917BECB858F63C,
    $3270B8FC5BF86DDA, $10AE72BB29B5DD76,
    $576AC94E7700362B, $1AD112DAC61EFB8F,
    $691BC30EC5FAA427, $FF246311CC327143,
    $3142368E30E53206, $71380E31E02CA396,
    $958D5C960AAD76F1, $F8D6F430C16DA536,
    $C8FFD13F1BE7E1D2, $7578AE66004DDBE1,
    $05833F01067BE646, $BB34B5AD3BFE586D,
    $095F34C9A12B97F0, $247AB64525D60CA8,
    $DCDBC6F3017477D1, $4A2E14D4DECAD24D,
    $BDB5E6D9BE0A1EEB, $2A7E70F7794301AB,
    $DEF42D8A270540FD, $01078EC0A34C22C1,
    $E5DE511AF4C16387, $7EBB3A52BD9A330A,
    $77697857AA7D6435, $004E831603AE4C32,
    $E7A21020AD78E312, $9D41A70C6AB420F2,
    $28E06C18EA1141E6, $D2B28CBD984F6B28,
    $26B75F6C446E9D83, $BA47568C4D418D7F,
    $D80BADBFE6183D8E, $0E206D7F5F166044,
    $E258A43911CBCA3E, $723A1746B21DC0BC,
    $C7CAA854F5D7CDD3, $7CAC32883D261D9C,
    $7690C26423BA942C, $17E55524478042B8,
    $E0BE477656A2389F, $4D289B5E67AB2DA0,
    $44862B9C8FBBFD31, $B47CC8049D141365,
    $822C1B362B91C793, $4EB14655FB13DFD8,
    $1ECBBA0714E2A97B, $6143459D5CDE5F14,
    $53A8FBF1D5F0AC89, $97EA04D81C5E5B00,
    $622181A8D4FDB3F3, $E9BCD341572A1208,
    $1411258643CCE58A, $9144C5FEA4C6E0A4,
    $0D33D06565CF620F, $54A48D489F219CA1,
    $C43E5EAC6D63C821, $A9728B3A72770DAF,
    $D7934E7B20DF87EF, $E35503B61A3E86E5,
    $CAE321FBC819D504, $129A50B3AC60BFA6,
    $CD5E68EA7E9FB6C3, $B01C90199483B1C7,
    $3DE93CD5C295376C, $AED52EDF2AB9AD13,
    $2E60F512C0A07884, $BC3D86A3E36210C9,
    $35269D9B163951CE, $0C7D6E2AD0CDB5FA,
    $59E86297D87F5733, $298EF221898DB0E7,
    $55000029D1A5AA7E, $8BC08AE1B5061B45,
    $C2C31C2B6C92703A, $94CC596BAF25EF42,
    $0A1D73DB22540456, $04B6A0F9D9C4179A,
    $EFFDAFA2AE3D3C60, $F7C8075BB49496C4,
    $9CC5C7141D1CD4E3, $78BD1638218E5534,
    $B2F11568F850246A, $EDFABCFA9502BC29,
    $796CE5F2DA23051B, $AAE128B0DC93537C,
    $3A493DA0EE4B29AE, $B5DF6B2C416895D7,
    $FCABBD25122D7F37, $70810B58105DC4B1,
    $E10FDD37F7882A90, $524DCAB5518A3F5C,
    $3C9E85878451255B, $4029828119BD34E2,
    $74A05B6F5D3CECCB, $B610021542E13ECA,
    $0FF979D12F59E2AC, $6037DA27E4F9CC50,
    $5E92975A0DF1847D, $D66DE190D3E623FE,
    $5032D6B87B568048, $9A36B7CE8235216E,
    $80272A7A24F64B4A, $93EFED8B8C6916F7,
    $37DDBFF44CCE1555, $4B95DB5D4B99BD25,
    $92D3FDA169812FC0, $FB1A4A9A90660BB6,
    $730C196946A4B9B2, $81E289AA7F49DA68,
    $64669A0F83B1A05F, $27B3FF7D9644F48B,
    $CC6B615C8DB675B3, $674F20B9BCEBBE95,
    $6F31238275655982, $5AE488713E45CF05,
    $BF619F9954C21157, $EABAC46040A8EAE9,
    $454C6FE9F2C0C1CD, $419CF6496412691C,
    $D3DC3BEF265B0F70, $6D0E60F5C3578A9E);

  tiger4            : array[0..255] of int64 = (
    $5B0E608526323C55, $1A46C1A9FA1B59F5,
    $A9E245A17C4C8FFA, $65CA5159DB2955D7,
    $05DB0A76CE35AFC2, $81EAC77EA9113D45,
    $528EF88AB6AC0A0D, $A09EA253597BE3FF,
    $430DDFB3AC48CD56, $C4B3A67AF45CE46F,
    $4ECECFD8FBE2D05E, $3EF56F10B39935F0,
    $0B22D6829CD619C6, $17FD460A74DF2069,
    $6CF8CC8E8510ED40, $D6C824BF3A6ECAA7,
    $61243D581A817049, $048BACB6BBC163A2,
    $D9A38AC27D44CC32, $7FDDFF5BAAF410AB,
    $AD6D495AA804824B, $E1A6A74F2D8C9F94,
    $D4F7851235DEE8E3, $FD4B7F886540D893,
    $247C20042AA4BFDA, $096EA1C517D1327C,
    $D56966B4361A6685, $277DA5C31221057D,
    $94D59893A43ACFF7, $64F0C51CCDC02281,
    $3D33BCC4FF6189DB, $E005CB184CE66AF1,
    $FF5CCD1D1DB99BEA, $B0B854A7FE42980F,
    $7BD46A6A718D4B9F, $D10FA8CC22A5FD8C,
    $D31484952BE4BD31, $C7FA975FCB243847,
    $4886ED1E5846C407, $28CDDB791EB70B04,
    $C2B00BE2F573417F, $5C9590452180F877,
    $7A6BDDFFF370EB00, $CE509E38D6D9D6A4,
    $EBEB0F00647FA702, $1DCC06CF76606F06,
    $E4D9F28BA286FF0A, $D85A305DC918C262,
    $475B1D8732225F54, $2D4FB51668CCB5FE,
    $A679B9D9D72BBA20, $53841C0D912D43A5,
    $3B7EAA48BF12A4E8, $781E0E47F22F1DDF,
    $EFF20CE60AB50973, $20D261D19DFFB742,
    $16A12B03062A2E39, $1960EB2239650495,
    $251C16FED50EB8B8, $9AC0C330F826016E,
    $ED152665953E7671, $02D63194A6369570,
    $5074F08394B1C987, $70BA598C90B25CE1,
    $794A15810B9742F6, $0D5925E9FCAF8C6C,
    $3067716CD868744E, $910AB077E8D7731B,
    $6A61BBDB5AC42F61, $93513EFBF0851567,
    $F494724B9E83E9D5, $E887E1985C09648D,
    $34B1D3C675370CFD, $DC35E433BC0D255D,
    $D0AAB84234131BE0, $08042A50B48B7EAF,
    $9997C4EE44A3AB35, $829A7B49201799D0,
    $263B8307B7C54441, $752F95F4FD6A6CA6,
    $927217402C08C6E5, $2A8AB754A795D9EE,
    $A442F7552F72943D, $2C31334E19781208,
    $4FA98D7CEAEE6291, $55C3862F665DB309,
    $BD0610175D53B1F3, $46FE6CB840413F27,
    $3FE03792DF0CFA59, $CFE700372EB85E8F,
    $A7BE29E7ADBCE118, $E544EE5CDE8431DD,
    $8A781B1B41F1873E, $A5C94C78A0D2F0E7,
    $39412E2877B60728, $A1265EF3AFC9A62C,
    $BCC2770C6A2506C5, $3AB66DD5DCE1CE12,
    $E65499D04A675B37, $7D8F523481BFD216,
    $0F6F64FCEC15F389, $74EFBE618B5B13C8,
    $ACDC82B714273E1D, $DD40BFE003199D17,
    $37E99257E7E061F8, $FA52626904775AAA,
    $8BBBF63A463D56F9, $F0013F1543A26E64,
    $A8307E9F879EC898, $CC4C27A4150177CC,
    $1B432F2CCA1D3348, $DE1D1F8F9F6FA013,
    $606602A047A7DDD6, $D237AB64CC1CB2C7,
    $9B938E7225FCD1D3, $EC4E03708E0FF476,
    $FEB2FBDA3D03C12D, $AE0BCED2EE43889A,
    $22CB8923EBFB4F43, $69360D013CF7396D,
    $855E3602D2D4E022, $073805BAD01F784C,
    $33E17A133852F546, $DF4874058AC7B638,
    $BA92B29C678AA14A, $0CE89FC76CFAADCD,
    $5F9D4E0908339E34, $F1AFE9291F5923B9,
    $6E3480F60F4A265F, $EEBF3A2AB29B841C,
    $E21938A88F91B4AD, $57DFEFF845C6D3C3,
    $2F006B0BF62CAAF2, $62F479EF6F75EE78,
    $11A55AD41C8916A9, $F229D29084FED453,
    $42F1C27B16B000E6, $2B1F76749823C074,
    $4B76ECA3C2745360, $8C98F463B91691BD,
    $14BCC93CF1ADE66A, $8885213E6D458397,
    $8E177DF0274D4711, $B49B73B5503F2951,
    $10168168C3F96B6B, $0E3D963B63CAB0AE,
    $8DFC4B5655A1DB14, $F789F1356E14DE5C,
    $683E68AF4E51DAC1, $C9A84F9D8D4B0FD9,
    $3691E03F52A0F9D1, $5ED86E46E1878E80,
    $3C711A0E99D07150, $5A0865B20C4E9310,
    $56FBFC1FE4F0682E, $EA8D5DE3105EDF9B,
    $71ABFDB12379187A, $2EB99DE1BEE77B9C,
    $21ECC0EA33CF4523, $59A4D7521805C7A1,
    $3896F5EB56AE7C72, $AA638F3DB18F75DC,
    $9F39358DABE9808E, $B7DEFA91C00B72AC,
    $6B5541FD62492D92, $6DC6DEE8F92E4D5B,
    $353F57ABC4BEEA7E, $735769D6DA5690CE,
    $0A234AA642391484, $F6F9508028F80D9D,
    $B8E319A27AB3F215, $31AD9C1151341A4D,
    $773C22A57BEF5805, $45C7561A07968633,
    $F913DA9E249DBE36, $DA652D9B78A64C68,
    $4C27A97F3BC334EF, $76621220E66B17F4,
    $967743899ACD7D0B, $F3EE5BCAE0ED6782,
    $409F753600C879FC, $06D09A39B5926DB6,
    $6F83AEB0317AC588, $01E6CA4A86381F21,
    $66FF3462D19F3025, $72207C24DDFD3BFB,
    $4AF6B6D3E2ECE2EB, $9C994DBEC7EA08DE,
    $49ACE597B09A8BC4, $B38C4766CF0797BA,
    $131B9373C57C2A75, $B1822CCE61931E58,
    $9D7555B909BA1C0C, $127FAFDD937D11D2,
    $29DA3BADC66D92E4, $A2C1D57154C2ECBC,
    $58C5134D82F6FE24, $1C3AE3515B62274F,
    $E907C82E01CB8126, $F8ED091913E37FCB,
    $3249D8F9C80046C9, $80CF9BEDE388FB63,
    $1881539A116CF19E, $5103F3F76BD52457,
    $15B7E6F5AE47F7A8, $DBD7C6DED47E9CCF,
    $44E55C410228BB1A, $B647D4255EDB4E99,
    $5D11882BB8AAFC30, $F5098BBB29D3212A,
    $8FB5EA14E90296B3, $677B942157DD025A,
    $FB58E7C0A390ACB5, $89D3674C83BD4A01,
    $9E2DA4DF4BF3B93B, $FCC41E328CAB4829,
    $03F38C96BA582C52, $CAD1BDBD7FD85DB2,
    $BBB442C16082AE83, $B95FE86BA5DA9AB0,
    $B22E04673771A93F, $845358C9493152D8,
    $BE2A488697B4541E, $95A2DC2DD38E6966,
    $C02C11AC923C852B, $2388B1990DF2A87B,
    $7C8008FA1B4F37BE, $1F70D0C84D54E503,
    $5490ADEC7ECE57D4, $002B3C27D9063A3A,
    $7EAEA3848030A2BF, $C602326DED2003C0,
    $83A7287D69A94086, $C57A5FCB30F57A8A,
    $B56844E479EBE779, $A373B40F05DCBCE9,
    $D71A786E88570EE2, $879CBACDBDE8F6A0,
    $976AD1BCC164A32F, $AB21E25E9666D78B,
    $901063AAE5E5C33C, $9818B34448698D90,
    $E36487AE3E1E8ABB, $AFBDF931893BDCB4,
    $6345A0DC5FBBD519, $8628FE269B9465CA,
    $1E5D01603F9C51EC, $4DE44006A15049B7,
    $BF6C70E5F776CBB1, $411218F2EF552BED,
    $CB0C0708705A36A3, $E74D14754F986044,
    $CD56D9430EA8280E, $C12591D7535F5065,
    $C83223F1720AEF96, $C3A0396F7363A51F);
{$WARNINGS ON}

type
  PTIGER = ^TTIGER;
  TTIGER = object(TObj)
  protected
    // code
    fInitialized: Boolean;
    Len: int64;
      Index: DWord;
    CurrentHash: array[0..2] of int64;
    HashBuffer: array[0..63] of byte;
    procedure Compress;

  public
    // code
    procedure InitHash;
    procedure Burn;
    procedure Update(const Buffer; Size: longword);
    procedure UpdateStr(const Str: string);
    procedure Final(var Digest);

    destructor Destroy; virtual;
  end;

  TKOLTIGER = PTIGER;

function NewHAVAL: PHAVAL;
function NewMD4: PMD4;
function NewMD5: PMD5;
function NewRMD128: PRMD128;
function NewRMD160: PRMD160;
function NewSHA1: PSHA1;
function NewSHA256: PSHA256;
function NewSHA384: PSHA384;
function NewSHA512: PSHA512;
function NewTIGER: PTIGER;

{ Choose how many passes (previous versions of DCPcrypt uses 5 passes) }
{ ONLY UNCOMMENT ONE! }
//{$DEFINE PASS3}
//{$DEFINE PASS4}
{$DEFINE PASS5}

{ Choose digest length (previous versions of DCPcrypt uses 256bits) }
{ ONLY UNCOMMENT ONE! }
//{$DEFINE DIGEST128}
//{$DEFINE DIGEST160}
//{$DEFINE DIGEST192}
//{$DEFINE DIGEST224}
{$DEFINE DIGEST256}

implementation
{$R-}{$Q-}

// uses CommCtrl, ShellApi;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor THAVAL.Destroy;
begin
  if fInitialized then
    Burn;

  // All Strings := '';
  // Free_And_Nil(All PObj);

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewHAVAL;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////
{$R-}{$Q-}

procedure THAVAL.Compress;
var
  t7, t6, t5, t4, t3, t2, t1, t0: DWord;
  W                 : array[0..31] of DWord;
  Temp              : dword;
begin
  t0 := CurrentHash[0];
  t1 := CurrentHash[1];
  t2 := CurrentHash[2];
  t3 := CurrentHash[3];
  t4 := CurrentHash[4];
  t5 := CurrentHash[5];
  t6 := CurrentHash[6];
  t7 := CurrentHash[7];
  Move(HashBuffer, W, Sizeof(W));

{$IFDEF PASS3}
  //{$INCLUDE DCPhaval3.inc}
  temp := (t2 and (t4 xor t3) xor t6 and t0 xor t5 and t1 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[0];
  temp := (t1 and (t3 xor t2) xor t5 and t7 xor t4 and t0 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[1];
  temp := (t0 and (t2 xor t1) xor t4 and t6 xor t3 and t7 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[2];
  temp := (t7 and (t1 xor t0) xor t3 and t5 xor t2 and t6 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[3];
  temp := (t6 and (t0 xor t7) xor t2 and t4 xor t1 and t5 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[4];
  temp := (t5 and (t7 xor t6) xor t1 and t3 xor t0 and t4 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[5];
  temp := (t4 and (t6 xor t5) xor t0 and t2 xor t7 and t3 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[6];
  temp := (t3 and (t5 xor t4) xor t7 and t1 xor t6 and t2 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[7];

  temp := (t2 and (t4 xor t3) xor t6 and t0 xor t5 and t1 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[8];
  temp := (t1 and (t3 xor t2) xor t5 and t7 xor t4 and t0 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[9];
  temp := (t0 and (t2 xor t1) xor t4 and t6 xor t3 and t7 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[10];
  temp := (t7 and (t1 xor t0) xor t3 and t5 xor t2 and t6 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[11];
  temp := (t6 and (t0 xor t7) xor t2 and t4 xor t1 and t5 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[12];
  temp := (t5 and (t7 xor t6) xor t1 and t3 xor t0 and t4 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[13];
  temp := (t4 and (t6 xor t5) xor t0 and t2 xor t7 and t3 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[14];
  temp := (t3 and (t5 xor t4) xor t7 and t1 xor t6 and t2 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[15];

  temp := (t2 and (t4 xor t3) xor t6 and t0 xor t5 and t1 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[16];
  temp := (t1 and (t3 xor t2) xor t5 and t7 xor t4 and t0 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[17];
  temp := (t0 and (t2 xor t1) xor t4 and t6 xor t3 and t7 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[18];
  temp := (t7 and (t1 xor t0) xor t3 and t5 xor t2 and t6 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[19];
  temp := (t6 and (t0 xor t7) xor t2 and t4 xor t1 and t5 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[20];
  temp := (t5 and (t7 xor t6) xor t1 and t3 xor t0 and t4 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[21];
  temp := (t4 and (t6 xor t5) xor t0 and t2 xor t7 and t3 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[22];
  temp := (t3 and (t5 xor t4) xor t7 and t1 xor t6 and t2 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[23];

  temp := (t2 and (t4 xor t3) xor t6 and t0 xor t5 and t1 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[24];
  temp := (t1 and (t3 xor t2) xor t5 and t7 xor t4 and t0 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[25];
  temp := (t0 and (t2 xor t1) xor t4 and t6 xor t3 and t7 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[26];
  temp := (t7 and (t1 xor t0) xor t3 and t5 xor t2 and t6 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[27];
  temp := (t6 and (t0 xor t7) xor t2 and t4 xor t1 and t5 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[28];
  temp := (t5 and (t7 xor t6) xor t1 and t3 xor t0 and t4 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[29];
  temp := (t4 and (t6 xor t5) xor t0 and t2 xor t7 and t3 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[30];
  temp := (t3 and (t5 xor t4) xor t7 and t1 xor t6 and t2 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[31];

  temp := (t5 and (t3 and not t0 xor t1 and t2 xor t4 xor t6) xor t1 and (t3 xor t2) xor t0 and t2 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[5] + $452821E6;
  temp := (t4 and (t2 and not t7 xor t0 and t1 xor t3 xor t5) xor t0 and (t2 xor t1) xor t7 and t1 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[14] + $38D01377;
  temp := (t3 and (t1 and not t6 xor t7 and t0 xor t2 xor t4) xor t7 and (t1 xor t0) xor t6 and t0 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[26] + $BE5466CF;
  temp := (t2 and (t0 and not t5 xor t6 and t7 xor t1 xor t3) xor t6 and (t0 xor t7) xor t5 and t7 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[18] + $34E90C6C;
  temp := (t1 and (t7 and not t4 xor t5 and t6 xor t0 xor t2) xor t5 and (t7 xor t6) xor t4 and t6 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[11] + $C0AC29B7;
  temp := (t0 and (t6 and not t3 xor t4 and t5 xor t7 xor t1) xor t4 and (t6 xor t5) xor t3 and t5 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[28] + $C97C50DD;
  temp := (t7 and (t5 and not t2 xor t3 and t4 xor t6 xor t0) xor t3 and (t5 xor t4) xor t2 and t4 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[7] + $3F84D5B5;
  temp := (t6 and (t4 and not t1 xor t2 and t3 xor t5 xor t7) xor t2 and (t4 xor t3) xor t1 and t3 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[16] + $B5470917;

  temp := (t5 and (t3 and not t0 xor t1 and t2 xor t4 xor t6) xor t1 and (t3 xor t2) xor t0 and t2 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[0] + $9216D5D9;
  temp := (t4 and (t2 and not t7 xor t0 and t1 xor t3 xor t5) xor t0 and (t2 xor t1) xor t7 and t1 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[23] + $8979FB1B;
  temp := (t3 and (t1 and not t6 xor t7 and t0 xor t2 xor t4) xor t7 and (t1 xor t0) xor t6 and t0 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[20] + $D1310BA6;
  temp := (t2 and (t0 and not t5 xor t6 and t7 xor t1 xor t3) xor t6 and (t0 xor t7) xor t5 and t7 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[22] + $98DFB5AC;
  temp := (t1 and (t7 and not t4 xor t5 and t6 xor t0 xor t2) xor t5 and (t7 xor t6) xor t4 and t6 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[1] + $2FFD72DB;
  temp := (t0 and (t6 and not t3 xor t4 and t5 xor t7 xor t1) xor t4 and (t6 xor t5) xor t3 and t5 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[10] + $D01ADFB7;
  temp := (t7 and (t5 and not t2 xor t3 and t4 xor t6 xor t0) xor t3 and (t5 xor t4) xor t2 and t4 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[4] + $B8E1AFED;
  temp := (t6 and (t4 and not t1 xor t2 and t3 xor t5 xor t7) xor t2 and (t4 xor t3) xor t1 and t3 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[8] + $6A267E96;

  temp := (t5 and (t3 and not t0 xor t1 and t2 xor t4 xor t6) xor t1 and (t3 xor t2) xor t0 and t2 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[30] + $BA7C9045;
  temp := (t4 and (t2 and not t7 xor t0 and t1 xor t3 xor t5) xor t0 and (t2 xor t1) xor t7 and t1 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[3] + $F12C7F99;
  temp := (t3 and (t1 and not t6 xor t7 and t0 xor t2 xor t4) xor t7 and (t1 xor t0) xor t6 and t0 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[21] + $24A19947;
  temp := (t2 and (t0 and not t5 xor t6 and t7 xor t1 xor t3) xor t6 and (t0 xor t7) xor t5 and t7 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[9] + $B3916CF7;
  temp := (t1 and (t7 and not t4 xor t5 and t6 xor t0 xor t2) xor t5 and (t7 xor t6) xor t4 and t6 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[17] + $0801F2E2;
  temp := (t0 and (t6 and not t3 xor t4 and t5 xor t7 xor t1) xor t4 and (t6 xor t5) xor t3 and t5 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[24] + $858EFC16;
  temp := (t7 and (t5 and not t2 xor t3 and t4 xor t6 xor t0) xor t3 and (t5 xor t4) xor t2 and t4 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[29] + $636920D8;
  temp := (t6 and (t4 and not t1 xor t2 and t3 xor t5 xor t7) xor t2 and (t4 xor t3) xor t1 and t3 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[6] + $71574E69;

  temp := (t5 and (t3 and not t0 xor t1 and t2 xor t4 xor t6) xor t1 and (t3 xor t2) xor t0 and t2 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[19] + $A458FEA3;
  temp := (t4 and (t2 and not t7 xor t0 and t1 xor t3 xor t5) xor t0 and (t2 xor t1) xor t7 and t1 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[12] + $F4933D7E;
  temp := (t3 and (t1 and not t6 xor t7 and t0 xor t2 xor t4) xor t7 and (t1 xor t0) xor t6 and t0 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[15] + $0D95748F;
  temp := (t2 and (t0 and not t5 xor t6 and t7 xor t1 xor t3) xor t6 and (t0 xor t7) xor t5 and t7 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[13] + $728EB658;
  temp := (t1 and (t7 and not t4 xor t5 and t6 xor t0 xor t2) xor t5 and (t7 xor t6) xor t4 and t6 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[2] + $718BCD58;
  temp := (t0 and (t6 and not t3 xor t4 and t5 xor t7 xor t1) xor t4 and (t6 xor t5) xor t3 and t5 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[25] + $82154AEE;
  temp := (t7 and (t5 and not t2 xor t3 and t4 xor t6 xor t0) xor t3 and (t5 xor t4) xor t2 and t4 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[31] + $7B54A41D;
  temp := (t6 and (t4 and not t1 xor t2 and t3 xor t5 xor t7) xor t2 and (t4 xor t3) xor t1 and t3 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[27] + $C25A59B5;

  temp := (t3 and (t5 and t4 xor t6 xor t0) xor t5 and t2 xor t4 and t1 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[19] + $9C30D539;
  temp := (t2 and (t4 and t3 xor t5 xor t7) xor t4 and t1 xor t3 and t0 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[9] + $2AF26013;
  temp := (t1 and (t3 and t2 xor t4 xor t6) xor t3 and t0 xor t2 and t7 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[4] + $C5D1B023;
  temp := (t0 and (t2 and t1 xor t3 xor t5) xor t2 and t7 xor t1 and t6 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[20] + $286085F0;
  temp := (t7 and (t1 and t0 xor t2 xor t4) xor t1 and t6 xor t0 and t5 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[28] + $CA417918;
  temp := (t6 and (t0 and t7 xor t1 xor t3) xor t0 and t5 xor t7 and t4 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[17] + $B8DB38EF;
  temp := (t5 and (t7 and t6 xor t0 xor t2) xor t7 and t4 xor t6 and t3 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[8] + $8E79DCB0;
  temp := (t4 and (t6 and t5 xor t7 xor t1) xor t6 and t3 xor t5 and t2 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[22] + $603A180E;

  temp := (t3 and (t5 and t4 xor t6 xor t0) xor t5 and t2 xor t4 and t1 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[29] + $6C9E0E8B;
  temp := (t2 and (t4 and t3 xor t5 xor t7) xor t4 and t1 xor t3 and t0 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[14] + $B01E8A3E;
  temp := (t1 and (t3 and t2 xor t4 xor t6) xor t3 and t0 xor t2 and t7 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[25] + $D71577C1;
  temp := (t0 and (t2 and t1 xor t3 xor t5) xor t2 and t7 xor t1 and t6 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[12] + $BD314B27;
  temp := (t7 and (t1 and t0 xor t2 xor t4) xor t1 and t6 xor t0 and t5 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[24] + $78AF2FDA;
  temp := (t6 and (t0 and t7 xor t1 xor t3) xor t0 and t5 xor t7 and t4 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[30] + $55605C60;
  temp := (t5 and (t7 and t6 xor t0 xor t2) xor t7 and t4 xor t6 and t3 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[16] + $E65525F3;
  temp := (t4 and (t6 and t5 xor t7 xor t1) xor t6 and t3 xor t5 and t2 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[26] + $AA55AB94;

  temp := (t3 and (t5 and t4 xor t6 xor t0) xor t5 and t2 xor t4 and t1 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[31] + $57489862;
  temp := (t2 and (t4 and t3 xor t5 xor t7) xor t4 and t1 xor t3 and t0 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[15] + $63E81440;
  temp := (t1 and (t3 and t2 xor t4 xor t6) xor t3 and t0 xor t2 and t7 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[7] + $55CA396A;
  temp := (t0 and (t2 and t1 xor t3 xor t5) xor t2 and t7 xor t1 and t6 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[3] + $2AAB10B6;
  temp := (t7 and (t1 and t0 xor t2 xor t4) xor t1 and t6 xor t0 and t5 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[1] + $B4CC5C34;
  temp := (t6 and (t0 and t7 xor t1 xor t3) xor t0 and t5 xor t7 and t4 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[0] + $1141E8CE;
  temp := (t5 and (t7 and t6 xor t0 xor t2) xor t7 and t4 xor t6 and t3 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[18] + $A15486AF;
  temp := (t4 and (t6 and t5 xor t7 xor t1) xor t6 and t3 xor t5 and t2 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[27] + $7C72E993;

  temp := (t3 and (t5 and t4 xor t6 xor t0) xor t5 and t2 xor t4 and t1 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[13] + $B3EE1411;
  temp := (t2 and (t4 and t3 xor t5 xor t7) xor t4 and t1 xor t3 and t0 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[6] + $636FBC2A;
  temp := (t1 and (t3 and t2 xor t4 xor t6) xor t3 and t0 xor t2 and t7 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[21] + $2BA9C55D;
  temp := (t0 and (t2 and t1 xor t3 xor t5) xor t2 and t7 xor t1 and t6 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[10] + $741831F6;
  temp := (t7 and (t1 and t0 xor t2 xor t4) xor t1 and t6 xor t0 and t5 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[23] + $CE5C3E16;
  temp := (t6 and (t0 and t7 xor t1 xor t3) xor t0 and t5 xor t7 and t4 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[11] + $9B87931E;
  temp := (t5 and (t7 and t6 xor t0 xor t2) xor t7 and t4 xor t6 and t3 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[5] + $AFD6BA33;
  temp := (t4 and (t6 and t5 xor t7 xor t1) xor t6 and t3 xor t5 and t2 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[2] + $6C24CF5C;

{$ELSE}
{$IFDEF PASS4}
  //    {$INCLUDE DCPhaval4.inc}
  temp := (t3 and (t0 xor t1) xor t5 and t6 xor t4 and t2 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[0];
  temp := (t2 and (t7 xor t0) xor t4 and t5 xor t3 and t1 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[1];
  temp := (t1 and (t6 xor t7) xor t3 and t4 xor t2 and t0 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[2];
  temp := (t0 and (t5 xor t6) xor t2 and t3 xor t1 and t7 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[3];
  temp := (t7 and (t4 xor t5) xor t1 and t2 xor t0 and t6 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[4];
  temp := (t6 and (t3 xor t4) xor t0 and t1 xor t7 and t5 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[5];
  temp := (t5 and (t2 xor t3) xor t7 and t0 xor t6 and t4 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[6];
  temp := (t4 and (t1 xor t2) xor t6 and t7 xor t5 and t3 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[7];

  temp := (t3 and (t0 xor t1) xor t5 and t6 xor t4 and t2 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[8];
  temp := (t2 and (t7 xor t0) xor t4 and t5 xor t3 and t1 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[9];
  temp := (t1 and (t6 xor t7) xor t3 and t4 xor t2 and t0 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[10];
  temp := (t0 and (t5 xor t6) xor t2 and t3 xor t1 and t7 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[11];
  temp := (t7 and (t4 xor t5) xor t1 and t2 xor t0 and t6 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[12];
  temp := (t6 and (t3 xor t4) xor t0 and t1 xor t7 and t5 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[13];
  temp := (t5 and (t2 xor t3) xor t7 and t0 xor t6 and t4 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[14];
  temp := (t4 and (t1 xor t2) xor t6 and t7 xor t5 and t3 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[15];

  temp := (t3 and (t0 xor t1) xor t5 and t6 xor t4 and t2 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[16];
  temp := (t2 and (t7 xor t0) xor t4 and t5 xor t3 and t1 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[17];
  temp := (t1 and (t6 xor t7) xor t3 and t4 xor t2 and t0 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[18];
  temp := (t0 and (t5 xor t6) xor t2 and t3 xor t1 and t7 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[19];
  temp := (t7 and (t4 xor t5) xor t1 and t2 xor t0 and t6 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[20];
  temp := (t6 and (t3 xor t4) xor t0 and t1 xor t7 and t5 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[21];
  temp := (t5 and (t2 xor t3) xor t7 and t0 xor t6 and t4 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[22];
  temp := (t4 and (t1 xor t2) xor t6 and t7 xor t5 and t3 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[23];

  temp := (t3 and (t0 xor t1) xor t5 and t6 xor t4 and t2 xor t0);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[24];
  temp := (t2 and (t7 xor t0) xor t4 and t5 xor t3 and t1 xor t7);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[25];
  temp := (t1 and (t6 xor t7) xor t3 and t4 xor t2 and t0 xor t6);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[26];
  temp := (t0 and (t5 xor t6) xor t2 and t3 xor t1 and t7 xor t5);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[27];
  temp := (t7 and (t4 xor t5) xor t1 and t2 xor t0 and t6 xor t4);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[28];
  temp := (t6 and (t3 xor t4) xor t0 and t1 xor t7 and t5 xor t3);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[29];
  temp := (t5 and (t2 xor t3) xor t7 and t0 xor t6 and t4 xor t2);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[30];
  temp := (t4 and (t1 xor t2) xor t6 and t7 xor t5 and t3 xor t1);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[31];

  temp := (t1 and (t6 and not t0 xor t2 and t5 xor t3 xor t4) xor t2 and (t6 xor t5) xor t0 and t5 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[5] + $452821E6;
  temp := (t0 and (t5 and not t7 xor t1 and t4 xor t2 xor t3) xor t1 and (t5 xor t4) xor t7 and t4 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[14] + $38D01377;
  temp := (t7 and (t4 and not t6 xor t0 and t3 xor t1 xor t2) xor t0 and (t4 xor t3) xor t6 and t3 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[26] + $BE5466CF;
  temp := (t6 and (t3 and not t5 xor t7 and t2 xor t0 xor t1) xor t7 and (t3 xor t2) xor t5 and t2 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[18] + $34E90C6C;
  temp := (t5 and (t2 and not t4 xor t6 and t1 xor t7 xor t0) xor t6 and (t2 xor t1) xor t4 and t1 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[11] + $C0AC29B7;
  temp := (t4 and (t1 and not t3 xor t5 and t0 xor t6 xor t7) xor t5 and (t1 xor t0) xor t3 and t0 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[28] + $C97C50DD;
  temp := (t3 and (t0 and not t2 xor t4 and t7 xor t5 xor t6) xor t4 and (t0 xor t7) xor t2 and t7 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[7] + $3F84D5B5;
  temp := (t2 and (t7 and not t1 xor t3 and t6 xor t4 xor t5) xor t3 and (t7 xor t6) xor t1 and t6 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[16] + $B5470917;

  temp := (t1 and (t6 and not t0 xor t2 and t5 xor t3 xor t4) xor t2 and (t6 xor t5) xor t0 and t5 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[0] + $9216D5D9;
  temp := (t0 and (t5 and not t7 xor t1 and t4 xor t2 xor t3) xor t1 and (t5 xor t4) xor t7 and t4 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[23] + $8979FB1B;
  temp := (t7 and (t4 and not t6 xor t0 and t3 xor t1 xor t2) xor t0 and (t4 xor t3) xor t6 and t3 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[20] + $D1310BA6;
  temp := (t6 and (t3 and not t5 xor t7 and t2 xor t0 xor t1) xor t7 and (t3 xor t2) xor t5 and t2 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[22] + $98DFB5AC;
  temp := (t5 and (t2 and not t4 xor t6 and t1 xor t7 xor t0) xor t6 and (t2 xor t1) xor t4 and t1 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[1] + $2FFD72DB;
  temp := (t4 and (t1 and not t3 xor t5 and t0 xor t6 xor t7) xor t5 and (t1 xor t0) xor t3 and t0 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[10] + $D01ADFB7;
  temp := (t3 and (t0 and not t2 xor t4 and t7 xor t5 xor t6) xor t4 and (t0 xor t7) xor t2 and t7 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[4] + $B8E1AFED;
  temp := (t2 and (t7 and not t1 xor t3 and t6 xor t4 xor t5) xor t3 and (t7 xor t6) xor t1 and t6 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[8] + $6A267E96;

  temp := (t1 and (t6 and not t0 xor t2 and t5 xor t3 xor t4) xor t2 and (t6 xor t5) xor t0 and t5 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[30] + $BA7C9045;
  temp := (t0 and (t5 and not t7 xor t1 and t4 xor t2 xor t3) xor t1 and (t5 xor t4) xor t7 and t4 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[3] + $F12C7F99;
  temp := (t7 and (t4 and not t6 xor t0 and t3 xor t1 xor t2) xor t0 and (t4 xor t3) xor t6 and t3 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[21] + $24A19947;
  temp := (t6 and (t3 and not t5 xor t7 and t2 xor t0 xor t1) xor t7 and (t3 xor t2) xor t5 and t2 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[9] + $B3916CF7;
  temp := (t5 and (t2 and not t4 xor t6 and t1 xor t7 xor t0) xor t6 and (t2 xor t1) xor t4 and t1 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[17] + $0801F2E2;
  temp := (t4 and (t1 and not t3 xor t5 and t0 xor t6 xor t7) xor t5 and (t1 xor t0) xor t3 and t0 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[24] + $858EFC16;
  temp := (t3 and (t0 and not t2 xor t4 and t7 xor t5 xor t6) xor t4 and (t0 xor t7) xor t2 and t7 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[29] + $636920D8;
  temp := (t2 and (t7 and not t1 xor t3 and t6 xor t4 xor t5) xor t3 and (t7 xor t6) xor t1 and t6 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[6] + $71574E69;

  temp := (t1 and (t6 and not t0 xor t2 and t5 xor t3 xor t4) xor t2 and (t6 xor t5) xor t0 and t5 xor t4);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[19] + $A458FEA3;
  temp := (t0 and (t5 and not t7 xor t1 and t4 xor t2 xor t3) xor t1 and (t5 xor t4) xor t7 and t4 xor t3);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[12] + $F4933D7E;
  temp := (t7 and (t4 and not t6 xor t0 and t3 xor t1 xor t2) xor t0 and (t4 xor t3) xor t6 and t3 xor t2);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[15] + $0D95748F;
  temp := (t6 and (t3 and not t5 xor t7 and t2 xor t0 xor t1) xor t7 and (t3 xor t2) xor t5 and t2 xor t1);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[13] + $728EB658;
  temp := (t5 and (t2 and not t4 xor t6 and t1 xor t7 xor t0) xor t6 and (t2 xor t1) xor t4 and t1 xor t0);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[2] + $718BCD58;
  temp := (t4 and (t1 and not t3 xor t5 and t0 xor t6 xor t7) xor t5 and (t1 xor t0) xor t3 and t0 xor t7);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[25] + $82154AEE;
  temp := (t3 and (t0 and not t2 xor t4 and t7 xor t5 xor t6) xor t4 and (t0 xor t7) xor t2 and t7 xor t6);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[31] + $7B54A41D;
  temp := (t2 and (t7 and not t1 xor t3 and t6 xor t4 xor t5) xor t3 and (t7 xor t6) xor t1 and t6 xor t5);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[27] + $C25A59B5;

  temp := (t6 and (t2 and t0 xor t1 xor t5) xor t2 and t3 xor t0 and t4 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[19] + $9C30D539;
  temp := (t5 and (t1 and t7 xor t0 xor t4) xor t1 and t2 xor t7 and t3 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[9] + $2AF26013;
  temp := (t4 and (t0 and t6 xor t7 xor t3) xor t0 and t1 xor t6 and t2 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[4] + $C5D1B023;
  temp := (t3 and (t7 and t5 xor t6 xor t2) xor t7 and t0 xor t5 and t1 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[20] + $286085F0;
  temp := (t2 and (t6 and t4 xor t5 xor t1) xor t6 and t7 xor t4 and t0 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[28] + $CA417918;
  temp := (t1 and (t5 and t3 xor t4 xor t0) xor t5 and t6 xor t3 and t7 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[17] + $B8DB38EF;
  temp := (t0 and (t4 and t2 xor t3 xor t7) xor t4 and t5 xor t2 and t6 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[8] + $8E79DCB0;
  temp := (t7 and (t3 and t1 xor t2 xor t6) xor t3 and t4 xor t1 and t5 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[22] + $603A180E;

  temp := (t6 and (t2 and t0 xor t1 xor t5) xor t2 and t3 xor t0 and t4 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[29] + $6C9E0E8B;
  temp := (t5 and (t1 and t7 xor t0 xor t4) xor t1 and t2 xor t7 and t3 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[14] + $B01E8A3E;
  temp := (t4 and (t0 and t6 xor t7 xor t3) xor t0 and t1 xor t6 and t2 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[25] + $D71577C1;
  temp := (t3 and (t7 and t5 xor t6 xor t2) xor t7 and t0 xor t5 and t1 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[12] + $BD314B27;
  temp := (t2 and (t6 and t4 xor t5 xor t1) xor t6 and t7 xor t4 and t0 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[24] + $78AF2FDA;
  temp := (t1 and (t5 and t3 xor t4 xor t0) xor t5 and t6 xor t3 and t7 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[30] + $55605C60;
  temp := (t0 and (t4 and t2 xor t3 xor t7) xor t4 and t5 xor t2 and t6 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[16] + $E65525F3;
  temp := (t7 and (t3 and t1 xor t2 xor t6) xor t3 and t4 xor t1 and t5 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[26] + $AA55AB94;

  temp := (t6 and (t2 and t0 xor t1 xor t5) xor t2 and t3 xor t0 and t4 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[31] + $57489862;
  temp := (t5 and (t1 and t7 xor t0 xor t4) xor t1 and t2 xor t7 and t3 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[15] + $63E81440;
  temp := (t4 and (t0 and t6 xor t7 xor t3) xor t0 and t1 xor t6 and t2 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[7] + $55CA396A;
  temp := (t3 and (t7 and t5 xor t6 xor t2) xor t7 and t0 xor t5 and t1 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[3] + $2AAB10B6;
  temp := (t2 and (t6 and t4 xor t5 xor t1) xor t6 and t7 xor t4 and t0 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[1] + $B4CC5C34;
  temp := (t1 and (t5 and t3 xor t4 xor t0) xor t5 and t6 xor t3 and t7 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[0] + $1141E8CE;
  temp := (t0 and (t4 and t2 xor t3 xor t7) xor t4 and t5 xor t2 and t6 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[18] + $A15486AF;
  temp := (t7 and (t3 and t1 xor t2 xor t6) xor t3 and t4 xor t1 and t5 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[27] + $7C72E993;

  temp := (t6 and (t2 and t0 xor t1 xor t5) xor t2 and t3 xor t0 and t4 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[13] + $B3EE1411;
  temp := (t5 and (t1 and t7 xor t0 xor t4) xor t1 and t2 xor t7 and t3 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[6] + $636FBC2A;
  temp := (t4 and (t0 and t6 xor t7 xor t3) xor t0 and t1 xor t6 and t2 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[21] + $2BA9C55D;
  temp := (t3 and (t7 and t5 xor t6 xor t2) xor t7 and t0 xor t5 and t1 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[10] + $741831F6;
  temp := (t2 and (t6 and t4 xor t5 xor t1) xor t6 and t7 xor t4 and t0 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[23] + $CE5C3E16;
  temp := (t1 and (t5 and t3 xor t4 xor t0) xor t5 and t6 xor t3 and t7 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[11] + $9B87931E;
  temp := (t0 and (t4 and t2 xor t3 xor t7) xor t4 and t5 xor t2 and t6 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[5] + $AFD6BA33;
  temp := (t7 and (t3 and t1 xor t2 xor t6) xor t3 and t4 xor t1 and t5 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[2] + $6C24CF5C;

  temp := (t0 and (t4 and not t2 xor t5 and not t6 xor t1 xor t6 xor t3) xor t5 and (t1 and t2 xor t4 xor t6) xor t2 and t6 xor t3);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[24] + $7A325381;
  temp := (t7 and (t3 and not t1 xor t4 and not t5 xor t0 xor t5 xor t2) xor t4 and (t0 and t1 xor t3 xor t5) xor t1 and t5 xor t2);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[4] + $28958677;
  temp := (t6 and (t2 and not t0 xor t3 and not t4 xor t7 xor t4 xor t1) xor t3 and (t7 and t0 xor t2 xor t4) xor t0 and t4 xor t1);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[0] + $3B8F4898;
  temp := (t5 and (t1 and not t7 xor t2 and not t3 xor t6 xor t3 xor t0) xor t2 and (t6 and t7 xor t1 xor t3) xor t7 and t3 xor t0);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[14] + $6B4BB9AF;
  temp := (t4 and (t0 and not t6 xor t1 and not t2 xor t5 xor t2 xor t7) xor t1 and (t5 and t6 xor t0 xor t2) xor t6 and t2 xor t7);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[2] + $C4BFE81B;
  temp := (t3 and (t7 and not t5 xor t0 and not t1 xor t4 xor t1 xor t6) xor t0 and (t4 and t5 xor t7 xor t1) xor t5 and t1 xor t6);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[7] + $66282193;
  temp := (t2 and (t6 and not t4 xor t7 and not t0 xor t3 xor t0 xor t5) xor t7 and (t3 and t4 xor t6 xor t0) xor t4 and t0 xor t5);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[28] + $61D809CC;
  temp := (t1 and (t5 and not t3 xor t6 and not t7 xor t2 xor t7 xor t4) xor t6 and (t2 and t3 xor t5 xor t7) xor t3 and t7 xor t4);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[23] + $FB21A991;

  temp := (t0 and (t4 and not t2 xor t5 and not t6 xor t1 xor t6 xor t3) xor t5 and (t1 and t2 xor t4 xor t6) xor t2 and t6 xor t3);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[26] + $487CAC60;
  temp := (t7 and (t3 and not t1 xor t4 and not t5 xor t0 xor t5 xor t2) xor t4 and (t0 and t1 xor t3 xor t5) xor t1 and t5 xor t2);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[6] + $5DEC8032;
  temp := (t6 and (t2 and not t0 xor t3 and not t4 xor t7 xor t4 xor t1) xor t3 and (t7 and t0 xor t2 xor t4) xor t0 and t4 xor t1);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[30] + $EF845D5D;
  temp := (t5 and (t1 and not t7 xor t2 and not t3 xor t6 xor t3 xor t0) xor t2 and (t6 and t7 xor t1 xor t3) xor t7 and t3 xor t0);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[20] + $E98575B1;
  temp := (t4 and (t0 and not t6 xor t1 and not t2 xor t5 xor t2 xor t7) xor t1 and (t5 and t6 xor t0 xor t2) xor t6 and t2 xor t7);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[18] + $DC262302;
  temp := (t3 and (t7 and not t5 xor t0 and not t1 xor t4 xor t1 xor t6) xor t0 and (t4 and t5 xor t7 xor t1) xor t5 and t1 xor t6);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[25] + $EB651B88;
  temp := (t2 and (t6 and not t4 xor t7 and not t0 xor t3 xor t0 xor t5) xor t7 and (t3 and t4 xor t6 xor t0) xor t4 and t0 xor t5);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[19] + $23893E81;
  temp := (t1 and (t5 and not t3 xor t6 and not t7 xor t2 xor t7 xor t4) xor t6 and (t2 and t3 xor t5 xor t7) xor t3 and t7 xor t4);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[3] + $D396ACC5;

  temp := (t0 and (t4 and not t2 xor t5 and not t6 xor t1 xor t6 xor t3) xor t5 and (t1 and t2 xor t4 xor t6) xor t2 and t6 xor t3);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[22] + $0F6D6FF3;
  temp := (t7 and (t3 and not t1 xor t4 and not t5 xor t0 xor t5 xor t2) xor t4 and (t0 and t1 xor t3 xor t5) xor t1 and t5 xor t2);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[11] + $83F44239;
  temp := (t6 and (t2 and not t0 xor t3 and not t4 xor t7 xor t4 xor t1) xor t3 and (t7 and t0 xor t2 xor t4) xor t0 and t4 xor t1);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[31] + $2E0B4482;
  temp := (t5 and (t1 and not t7 xor t2 and not t3 xor t6 xor t3 xor t0) xor t2 and (t6 and t7 xor t1 xor t3) xor t7 and t3 xor t0);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[21] + $A4842004;
  temp := (t4 and (t0 and not t6 xor t1 and not t2 xor t5 xor t2 xor t7) xor t1 and (t5 and t6 xor t0 xor t2) xor t6 and t2 xor t7);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[8] + $69C8F04A;
  temp := (t3 and (t7 and not t5 xor t0 and not t1 xor t4 xor t1 xor t6) xor t0 and (t4 and t5 xor t7 xor t1) xor t5 and t1 xor t6);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[27] + $9E1F9B5E;
  temp := (t2 and (t6 and not t4 xor t7 and not t0 xor t3 xor t0 xor t5) xor t7 and (t3 and t4 xor t6 xor t0) xor t4 and t0 xor t5);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[12] + $21C66842;
  temp := (t1 and (t5 and not t3 xor t6 and not t7 xor t2 xor t7 xor t4) xor t6 and (t2 and t3 xor t5 xor t7) xor t3 and t7 xor t4);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[9] + $F6E96C9A;

  temp := (t0 and (t4 and not t2 xor t5 and not t6 xor t1 xor t6 xor t3) xor t5 and (t1 and t2 xor t4 xor t6) xor t2 and t6 xor t3);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[1] + $670C9C61;
  temp := (t7 and (t3 and not t1 xor t4 and not t5 xor t0 xor t5 xor t2) xor t4 and (t0 and t1 xor t3 xor t5) xor t1 and t5 xor t2);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[29] + $ABD388F0;
  temp := (t6 and (t2 and not t0 xor t3 and not t4 xor t7 xor t4 xor t1) xor t3 and (t7 and t0 xor t2 xor t4) xor t0 and t4 xor t1);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[5] + $6A51A0D2;
  temp := (t5 and (t1 and not t7 xor t2 and not t3 xor t6 xor t3 xor t0) xor t2 and (t6 and t7 xor t1 xor t3) xor t7 and t3 xor t0);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[15] + $D8542F68;
  temp := (t4 and (t0 and not t6 xor t1 and not t2 xor t5 xor t2 xor t7) xor t1 and (t5 and t6 xor t0 xor t2) xor t6 and t2 xor t7);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[17] + $960FA728;
  temp := (t3 and (t7 and not t5 xor t0 and not t1 xor t4 xor t1 xor t6) xor t0 and (t4 and t5 xor t7 xor t1) xor t5 and t1 xor t6);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[10] + $AB5133A3;
  temp := (t2 and (t6 and not t4 xor t7 and not t0 xor t3 xor t0 xor t5) xor t7 and (t3 and t4 xor t6 xor t0) xor t4 and t0 xor t5);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[16] + $6EEF0B6C;
  temp := (t1 and (t5 and not t3 xor t6 and not t7 xor t2 xor t7 xor t4) xor t6 and (t2 and t3 xor t5 xor t7) xor t3 and t7 xor t4);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[13] + $137A3BE4;

{$ELSE}
  //    {$INCLUDE DCPhaval5.inc}
  temp := (t2 and (t6 xor t1) xor t5 and t4 xor t0 and t3 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[0];
  temp := (t1 and (t5 xor t0) xor t4 and t3 xor t7 and t2 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[1];
  temp := (t0 and (t4 xor t7) xor t3 and t2 xor t6 and t1 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[2];
  temp := (t7 and (t3 xor t6) xor t2 and t1 xor t5 and t0 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[3];
  temp := (t6 and (t2 xor t5) xor t1 and t0 xor t4 and t7 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[4];
  temp := (t5 and (t1 xor t4) xor t0 and t7 xor t3 and t6 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[5];
  temp := (t4 and (t0 xor t3) xor t7 and t6 xor t2 and t5 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[6];
  temp := (t3 and (t7 xor t2) xor t6 and t5 xor t1 and t4 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[7];

  temp := (t2 and (t6 xor t1) xor t5 and t4 xor t0 and t3 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[8];
  temp := (t1 and (t5 xor t0) xor t4 and t3 xor t7 and t2 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[9];
  temp := (t0 and (t4 xor t7) xor t3 and t2 xor t6 and t1 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[10];
  temp := (t7 and (t3 xor t6) xor t2 and t1 xor t5 and t0 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[11];
  temp := (t6 and (t2 xor t5) xor t1 and t0 xor t4 and t7 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[12];
  temp := (t5 and (t1 xor t4) xor t0 and t7 xor t3 and t6 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[13];
  temp := (t4 and (t0 xor t3) xor t7 and t6 xor t2 and t5 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[14];
  temp := (t3 and (t7 xor t2) xor t6 and t5 xor t1 and t4 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[15];

  temp := (t2 and (t6 xor t1) xor t5 and t4 xor t0 and t3 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[16];
  temp := (t1 and (t5 xor t0) xor t4 and t3 xor t7 and t2 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[17];
  temp := (t0 and (t4 xor t7) xor t3 and t2 xor t6 and t1 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[18];
  temp := (t7 and (t3 xor t6) xor t2 and t1 xor t5 and t0 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[19];
  temp := (t6 and (t2 xor t5) xor t1 and t0 xor t4 and t7 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[20];
  temp := (t5 and (t1 xor t4) xor t0 and t7 xor t3 and t6 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[21];
  temp := (t4 and (t0 xor t3) xor t7 and t6 xor t2 and t5 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[22];
  temp := (t3 and (t7 xor t2) xor t6 and t5 xor t1 and t4 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[23];

  temp := (t2 and (t6 xor t1) xor t5 and t4 xor t0 and t3 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[24];
  temp := (t1 and (t5 xor t0) xor t4 and t3 xor t7 and t2 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[25];
  temp := (t0 and (t4 xor t7) xor t3 and t2 xor t6 and t1 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[26];
  temp := (t7 and (t3 xor t6) xor t2 and t1 xor t5 and t0 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[27];
  temp := (t6 and (t2 xor t5) xor t1 and t0 xor t4 and t7 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[28];
  temp := (t5 and (t1 xor t4) xor t0 and t7 xor t3 and t6 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[29];
  temp := (t4 and (t0 xor t3) xor t7 and t6 xor t2 and t5 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[30];
  temp := (t3 and (t7 xor t2) xor t6 and t5 xor t1 and t4 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[31];

  temp := (t3 and (t4 and not t0 xor t1 and t2 xor t6 xor t5) xor t1 and (t4 xor t2) xor t0 and t2 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[5] + $452821E6;
  temp := (t2 and (t3 and not t7 xor t0 and t1 xor t5 xor t4) xor t0 and (t3 xor t1) xor t7 and t1 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[14] + $38D01377;
  temp := (t1 and (t2 and not t6 xor t7 and t0 xor t4 xor t3) xor t7 and (t2 xor t0) xor t6 and t0 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[26] + $BE5466CF;
  temp := (t0 and (t1 and not t5 xor t6 and t7 xor t3 xor t2) xor t6 and (t1 xor t7) xor t5 and t7 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[18] + $34E90C6C;
  temp := (t7 and (t0 and not t4 xor t5 and t6 xor t2 xor t1) xor t5 and (t0 xor t6) xor t4 and t6 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[11] + $C0AC29B7;
  temp := (t6 and (t7 and not t3 xor t4 and t5 xor t1 xor t0) xor t4 and (t7 xor t5) xor t3 and t5 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[28] + $C97C50DD;
  temp := (t5 and (t6 and not t2 xor t3 and t4 xor t0 xor t7) xor t3 and (t6 xor t4) xor t2 and t4 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[7] + $3F84D5B5;
  temp := (t4 and (t5 and not t1 xor t2 and t3 xor t7 xor t6) xor t2 and (t5 xor t3) xor t1 and t3 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[16] + $B5470917;

  temp := (t3 and (t4 and not t0 xor t1 and t2 xor t6 xor t5) xor t1 and (t4 xor t2) xor t0 and t2 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[0] + $9216D5D9;
  temp := (t2 and (t3 and not t7 xor t0 and t1 xor t5 xor t4) xor t0 and (t3 xor t1) xor t7 and t1 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[23] + $8979FB1B;
  temp := (t1 and (t2 and not t6 xor t7 and t0 xor t4 xor t3) xor t7 and (t2 xor t0) xor t6 and t0 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[20] + $D1310BA6;
  temp := (t0 and (t1 and not t5 xor t6 and t7 xor t3 xor t2) xor t6 and (t1 xor t7) xor t5 and t7 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[22] + $98DFB5AC;
  temp := (t7 and (t0 and not t4 xor t5 and t6 xor t2 xor t1) xor t5 and (t0 xor t6) xor t4 and t6 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[1] + $2FFD72DB;
  temp := (t6 and (t7 and not t3 xor t4 and t5 xor t1 xor t0) xor t4 and (t7 xor t5) xor t3 and t5 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[10] + $D01ADFB7;
  temp := (t5 and (t6 and not t2 xor t3 and t4 xor t0 xor t7) xor t3 and (t6 xor t4) xor t2 and t4 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[4] + $B8E1AFED;
  temp := (t4 and (t5 and not t1 xor t2 and t3 xor t7 xor t6) xor t2 and (t5 xor t3) xor t1 and t3 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[8] + $6A267E96;

  temp := (t3 and (t4 and not t0 xor t1 and t2 xor t6 xor t5) xor t1 and (t4 xor t2) xor t0 and t2 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[30] + $BA7C9045;
  temp := (t2 and (t3 and not t7 xor t0 and t1 xor t5 xor t4) xor t0 and (t3 xor t1) xor t7 and t1 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[3] + $F12C7F99;
  temp := (t1 and (t2 and not t6 xor t7 and t0 xor t4 xor t3) xor t7 and (t2 xor t0) xor t6 and t0 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[21] + $24A19947;
  temp := (t0 and (t1 and not t5 xor t6 and t7 xor t3 xor t2) xor t6 and (t1 xor t7) xor t5 and t7 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[9] + $B3916CF7;
  temp := (t7 and (t0 and not t4 xor t5 and t6 xor t2 xor t1) xor t5 and (t0 xor t6) xor t4 and t6 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[17] + $0801F2E2;
  temp := (t6 and (t7 and not t3 xor t4 and t5 xor t1 xor t0) xor t4 and (t7 xor t5) xor t3 and t5 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[24] + $858EFC16;
  temp := (t5 and (t6 and not t2 xor t3 and t4 xor t0 xor t7) xor t3 and (t6 xor t4) xor t2 and t4 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[29] + $636920D8;
  temp := (t4 and (t5 and not t1 xor t2 and t3 xor t7 xor t6) xor t2 and (t5 xor t3) xor t1 and t3 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[6] + $71574E69;

  temp := (t3 and (t4 and not t0 xor t1 and t2 xor t6 xor t5) xor t1 and (t4 xor t2) xor t0 and t2 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[19] + $A458FEA3;
  temp := (t2 and (t3 and not t7 xor t0 and t1 xor t5 xor t4) xor t0 and (t3 xor t1) xor t7 and t1 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[12] + $F4933D7E;
  temp := (t1 and (t2 and not t6 xor t7 and t0 xor t4 xor t3) xor t7 and (t2 xor t0) xor t6 and t0 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[15] + $0D95748F;
  temp := (t0 and (t1 and not t5 xor t6 and t7 xor t3 xor t2) xor t6 and (t1 xor t7) xor t5 and t7 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[13] + $728EB658;
  temp := (t7 and (t0 and not t4 xor t5 and t6 xor t2 xor t1) xor t5 and (t0 xor t6) xor t4 and t6 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[2] + $718BCD58;
  temp := (t6 and (t7 and not t3 xor t4 and t5 xor t1 xor t0) xor t4 and (t7 xor t5) xor t3 and t5 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[25] + $82154AEE;
  temp := (t5 and (t6 and not t2 xor t3 and t4 xor t0 xor t7) xor t3 and (t6 xor t4) xor t2 and t4 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[31] + $7B54A41D;
  temp := (t4 and (t5 and not t1 xor t2 and t3 xor t7 xor t6) xor t2 and (t5 xor t3) xor t1 and t3 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[27] + $C25A59B5;

  temp := (t4 and (t1 and t3 xor t2 xor t5) xor t1 and t0 xor t3 and t6 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[19] + $9C30D539;
  temp := (t3 and (t0 and t2 xor t1 xor t4) xor t0 and t7 xor t2 and t5 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[9] + $2AF26013;
  temp := (t2 and (t7 and t1 xor t0 xor t3) xor t7 and t6 xor t1 and t4 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[4] + $C5D1B023;
  temp := (t1 and (t6 and t0 xor t7 xor t2) xor t6 and t5 xor t0 and t3 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[20] + $286085F0;
  temp := (t0 and (t5 and t7 xor t6 xor t1) xor t5 and t4 xor t7 and t2 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[28] + $CA417918;
  temp := (t7 and (t4 and t6 xor t5 xor t0) xor t4 and t3 xor t6 and t1 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[17] + $B8DB38EF;
  temp := (t6 and (t3 and t5 xor t4 xor t7) xor t3 and t2 xor t5 and t0 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[8] + $8E79DCB0;
  temp := (t5 and (t2 and t4 xor t3 xor t6) xor t2 and t1 xor t4 and t7 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[22] + $603A180E;

  temp := (t4 and (t1 and t3 xor t2 xor t5) xor t1 and t0 xor t3 and t6 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[29] + $6C9E0E8B;
  temp := (t3 and (t0 and t2 xor t1 xor t4) xor t0 and t7 xor t2 and t5 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[14] + $B01E8A3E;
  temp := (t2 and (t7 and t1 xor t0 xor t3) xor t7 and t6 xor t1 and t4 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[25] + $D71577C1;
  temp := (t1 and (t6 and t0 xor t7 xor t2) xor t6 and t5 xor t0 and t3 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[12] + $BD314B27;
  temp := (t0 and (t5 and t7 xor t6 xor t1) xor t5 and t4 xor t7 and t2 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[24] + $78AF2FDA;
  temp := (t7 and (t4 and t6 xor t5 xor t0) xor t4 and t3 xor t6 and t1 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[30] + $55605C60;
  temp := (t6 and (t3 and t5 xor t4 xor t7) xor t3 and t2 xor t5 and t0 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[16] + $E65525F3;
  temp := (t5 and (t2 and t4 xor t3 xor t6) xor t2 and t1 xor t4 and t7 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[26] + $AA55AB94;

  temp := (t4 and (t1 and t3 xor t2 xor t5) xor t1 and t0 xor t3 and t6 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[31] + $57489862;
  temp := (t3 and (t0 and t2 xor t1 xor t4) xor t0 and t7 xor t2 and t5 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[15] + $63E81440;
  temp := (t2 and (t7 and t1 xor t0 xor t3) xor t7 and t6 xor t1 and t4 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[7] + $55CA396A;
  temp := (t1 and (t6 and t0 xor t7 xor t2) xor t6 and t5 xor t0 and t3 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[3] + $2AAB10B6;
  temp := (t0 and (t5 and t7 xor t6 xor t1) xor t5 and t4 xor t7 and t2 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[1] + $B4CC5C34;
  temp := (t7 and (t4 and t6 xor t5 xor t0) xor t4 and t3 xor t6 and t1 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[0] + $1141E8CE;
  temp := (t6 and (t3 and t5 xor t4 xor t7) xor t3 and t2 xor t5 and t0 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[18] + $A15486AF;
  temp := (t5 and (t2 and t4 xor t3 xor t6) xor t2 and t1 xor t4 and t7 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[27] + $7C72E993;

  temp := (t4 and (t1 and t3 xor t2 xor t5) xor t1 and t0 xor t3 and t6 xor t5);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[13] + $B3EE1411;
  temp := (t3 and (t0 and t2 xor t1 xor t4) xor t0 and t7 xor t2 and t5 xor t4);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[6] + $636FBC2A;
  temp := (t2 and (t7 and t1 xor t0 xor t3) xor t7 and t6 xor t1 and t4 xor t3);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[21] + $2BA9C55D;
  temp := (t1 and (t6 and t0 xor t7 xor t2) xor t6 and t5 xor t0 and t3 xor t2);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[10] + $741831F6;
  temp := (t0 and (t5 and t7 xor t6 xor t1) xor t5 and t4 xor t7 and t2 xor t1);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[23] + $CE5C3E16;
  temp := (t7 and (t4 and t6 xor t5 xor t0) xor t4 and t3 xor t6 and t1 xor t0);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[11] + $9B87931E;
  temp := (t6 and (t3 and t5 xor t4 xor t7) xor t3 and t2 xor t5 and t0 xor t7);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[5] + $AFD6BA33;
  temp := (t5 and (t2 and t4 xor t3 xor t6) xor t2 and t1 xor t4 and t7 xor t6);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[2] + $6C24CF5C;

  temp := (t3 and (t5 and not t0 xor t2 and not t1 xor t4 xor t1 xor t6) xor t2 and (t4 and t0 xor t5 xor t1) xor t0 and t1 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[24] + $7A325381;
  temp := (t2 and (t4 and not t7 xor t1 and not t0 xor t3 xor t0 xor t5) xor t1 and (t3 and t7 xor t4 xor t0) xor t7 and t0 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[4] + $28958677;
  temp := (t1 and (t3 and not t6 xor t0 and not t7 xor t2 xor t7 xor t4) xor t0 and (t2 and t6 xor t3 xor t7) xor t6 and t7 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[0] + $3B8F4898;
  temp := (t0 and (t2 and not t5 xor t7 and not t6 xor t1 xor t6 xor t3) xor t7 and (t1 and t5 xor t2 xor t6) xor t5 and t6 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[14] + $6B4BB9AF;
  temp := (t7 and (t1 and not t4 xor t6 and not t5 xor t0 xor t5 xor t2) xor t6 and (t0 and t4 xor t1 xor t5) xor t4 and t5 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[2] + $C4BFE81B;
  temp := (t6 and (t0 and not t3 xor t5 and not t4 xor t7 xor t4 xor t1) xor t5 and (t7 and t3 xor t0 xor t4) xor t3 and t4 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[7] + $66282193;
  temp := (t5 and (t7 and not t2 xor t4 and not t3 xor t6 xor t3 xor t0) xor t4 and (t6 and t2 xor t7 xor t3) xor t2 and t3 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[28] + $61D809CC;
  temp := (t4 and (t6 and not t1 xor t3 and not t2 xor t5 xor t2 xor t7) xor t3 and (t5 and t1 xor t6 xor t2) xor t1 and t2 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[23] + $FB21A991;

  temp := (t3 and (t5 and not t0 xor t2 and not t1 xor t4 xor t1 xor t6) xor t2 and (t4 and t0 xor t5 xor t1) xor t0 and t1 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[26] + $487CAC60;
  temp := (t2 and (t4 and not t7 xor t1 and not t0 xor t3 xor t0 xor t5) xor t1 and (t3 and t7 xor t4 xor t0) xor t7 and t0 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[6] + $5DEC8032;
  temp := (t1 and (t3 and not t6 xor t0 and not t7 xor t2 xor t7 xor t4) xor t0 and (t2 and t6 xor t3 xor t7) xor t6 and t7 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[30] + $EF845D5D;
  temp := (t0 and (t2 and not t5 xor t7 and not t6 xor t1 xor t6 xor t3) xor t7 and (t1 and t5 xor t2 xor t6) xor t5 and t6 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[20] + $E98575B1;
  temp := (t7 and (t1 and not t4 xor t6 and not t5 xor t0 xor t5 xor t2) xor t6 and (t0 and t4 xor t1 xor t5) xor t4 and t5 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[18] + $DC262302;
  temp := (t6 and (t0 and not t3 xor t5 and not t4 xor t7 xor t4 xor t1) xor t5 and (t7 and t3 xor t0 xor t4) xor t3 and t4 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[25] + $EB651B88;
  temp := (t5 and (t7 and not t2 xor t4 and not t3 xor t6 xor t3 xor t0) xor t4 and (t6 and t2 xor t7 xor t3) xor t2 and t3 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[19] + $23893E81;
  temp := (t4 and (t6 and not t1 xor t3 and not t2 xor t5 xor t2 xor t7) xor t3 and (t5 and t1 xor t6 xor t2) xor t1 and t2 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[3] + $D396ACC5;

  temp := (t3 and (t5 and not t0 xor t2 and not t1 xor t4 xor t1 xor t6) xor t2 and (t4 and t0 xor t5 xor t1) xor t0 and t1 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[22] + $0F6D6FF3;
  temp := (t2 and (t4 and not t7 xor t1 and not t0 xor t3 xor t0 xor t5) xor t1 and (t3 and t7 xor t4 xor t0) xor t7 and t0 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[11] + $83F44239;
  temp := (t1 and (t3 and not t6 xor t0 and not t7 xor t2 xor t7 xor t4) xor t0 and (t2 and t6 xor t3 xor t7) xor t6 and t7 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[31] + $2E0B4482;
  temp := (t0 and (t2 and not t5 xor t7 and not t6 xor t1 xor t6 xor t3) xor t7 and (t1 and t5 xor t2 xor t6) xor t5 and t6 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[21] + $A4842004;
  temp := (t7 and (t1 and not t4 xor t6 and not t5 xor t0 xor t5 xor t2) xor t6 and (t0 and t4 xor t1 xor t5) xor t4 and t5 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[8] + $69C8F04A;
  temp := (t6 and (t0 and not t3 xor t5 and not t4 xor t7 xor t4 xor t1) xor t5 and (t7 and t3 xor t0 xor t4) xor t3 and t4 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[27] + $9E1F9B5E;
  temp := (t5 and (t7 and not t2 xor t4 and not t3 xor t6 xor t3 xor t0) xor t4 and (t6 and t2 xor t7 xor t3) xor t2 and t3 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[12] + $21C66842;
  temp := (t4 and (t6 and not t1 xor t3 and not t2 xor t5 xor t2 xor t7) xor t3 and (t5 and t1 xor t6 xor t2) xor t1 and t2 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[9] + $F6E96C9A;

  temp := (t3 and (t5 and not t0 xor t2 and not t1 xor t4 xor t1 xor t6) xor t2 and (t4 and t0 xor t5 xor t1) xor t0 and t1 xor t6);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[1] + $670C9C61;
  temp := (t2 and (t4 and not t7 xor t1 and not t0 xor t3 xor t0 xor t5) xor t1 and (t3 and t7 xor t4 xor t0) xor t7 and t0 xor t5);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[29] + $ABD388F0;
  temp := (t1 and (t3 and not t6 xor t0 and not t7 xor t2 xor t7 xor t4) xor t0 and (t2 and t6 xor t3 xor t7) xor t6 and t7 xor t4);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[5] + $6A51A0D2;
  temp := (t0 and (t2 and not t5 xor t7 and not t6 xor t1 xor t6 xor t3) xor t7 and (t1 and t5 xor t2 xor t6) xor t5 and t6 xor t3);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[15] + $D8542F68;
  temp := (t7 and (t1 and not t4 xor t6 and not t5 xor t0 xor t5 xor t2) xor t6 and (t0 and t4 xor t1 xor t5) xor t4 and t5 xor t2);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[17] + $960FA728;
  temp := (t6 and (t0 and not t3 xor t5 and not t4 xor t7 xor t4 xor t1) xor t5 and (t7 and t3 xor t0 xor t4) xor t3 and t4 xor t1);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[10] + $AB5133A3;
  temp := (t5 and (t7 and not t2 xor t4 and not t3 xor t6 xor t3 xor t0) xor t4 and (t6 and t2 xor t7 xor t3) xor t2 and t3 xor t0);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[16] + $6EEF0B6C;
  temp := (t4 and (t6 and not t1 xor t3 and not t2 xor t5 xor t2 xor t7) xor t3 and (t5 and t1 xor t6 xor t2) xor t1 and t2 xor t7);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[13] + $137A3BE4;

  temp := (t1 and (t3 and t4 and t6 xor not t5) xor t3 and t0 xor t4 and t5 xor t6 and t2);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[27] + $BA3BF050;
  temp := (t0 and (t2 and t3 and t5 xor not t4) xor t2 and t7 xor t3 and t4 xor t5 and t1);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[3] + $7EFB2A98;
  temp := (t7 and (t1 and t2 and t4 xor not t3) xor t1 and t6 xor t2 and t3 xor t4 and t0);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[21] + $A1F1651D;
  temp := (t6 and (t0 and t1 and t3 xor not t2) xor t0 and t5 xor t1 and t2 xor t3 and t7);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[26] + $39AF0176;
  temp := (t5 and (t7 and t0 and t2 xor not t1) xor t7 and t4 xor t0 and t1 xor t2 and t6);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[17] + $66CA593E;
  temp := (t4 and (t6 and t7 and t1 xor not t0) xor t6 and t3 xor t7 and t0 xor t1 and t5);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[11] + $82430E88;
  temp := (t3 and (t5 and t6 and t0 xor not t7) xor t5 and t2 xor t6 and t7 xor t0 and t4);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[20] + $8CEE8619;
  temp := (t2 and (t4 and t5 and t7 xor not t6) xor t4 and t1 xor t5 and t6 xor t7 and t3);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[29] + $456F9FB4;

  temp := (t1 and (t3 and t4 and t6 xor not t5) xor t3 and t0 xor t4 and t5 xor t6 and t2);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[19] + $7D84A5C3;
  temp := (t0 and (t2 and t3 and t5 xor not t4) xor t2 and t7 xor t3 and t4 xor t5 and t1);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[0] + $3B8B5EBE;
  temp := (t7 and (t1 and t2 and t4 xor not t3) xor t1 and t6 xor t2 and t3 xor t4 and t0);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[12] + $E06F75D8;
  temp := (t6 and (t0 and t1 and t3 xor not t2) xor t0 and t5 xor t1 and t2 xor t3 and t7);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[7] + $85C12073;
  temp := (t5 and (t7 and t0 and t2 xor not t1) xor t7 and t4 xor t0 and t1 xor t2 and t6);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[13] + $401A449F;
  temp := (t4 and (t6 and t7 and t1 xor not t0) xor t6 and t3 xor t7 and t0 xor t1 and t5);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[8] + $56C16AA6;
  temp := (t3 and (t5 and t6 and t0 xor not t7) xor t5 and t2 xor t6 and t7 xor t0 and t4);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[31] + $4ED3AA62;
  temp := (t2 and (t4 and t5 and t7 xor not t6) xor t4 and t1 xor t5 and t6 xor t7 and t3);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[10] + $363F7706;

  temp := (t1 and (t3 and t4 and t6 xor not t5) xor t3 and t0 xor t4 and t5 xor t6 and t2);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[5] + $1BFEDF72;
  temp := (t0 and (t2 and t3 and t5 xor not t4) xor t2 and t7 xor t3 and t4 xor t5 and t1);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[9] + $429B023D;
  temp := (t7 and (t1 and t2 and t4 xor not t3) xor t1 and t6 xor t2 and t3 xor t4 and t0);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[14] + $37D0D724;
  temp := (t6 and (t0 and t1 and t3 xor not t2) xor t0 and t5 xor t1 and t2 xor t3 and t7);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[30] + $D00A1248;
  temp := (t5 and (t7 and t0 and t2 xor not t1) xor t7 and t4 xor t0 and t1 xor t2 and t6);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[18] + $DB0FEAD3;
  temp := (t4 and (t6 and t7 and t1 xor not t0) xor t6 and t3 xor t7 and t0 xor t1 and t5);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[6] + $49F1C09B;
  temp := (t3 and (t5 and t6 and t0 xor not t7) xor t5 and t2 xor t6 and t7 xor t0 and t4);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[28] + $075372C9;
  temp := (t2 and (t4 and t5 and t7 xor not t6) xor t4 and t1 xor t5 and t6 xor t7 and t3);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[24] + $80991B7B;

  temp := (t1 and (t3 and t4 and t6 xor not t5) xor t3 and t0 xor t4 and t5 xor t6 and t2);
  t7 := ((temp shr 7) or (temp shl 25)) + ((t7 shr 11) or (t7 shl 21)) + w[2] + $25D479D8;
  temp := (t0 and (t2 and t3 and t5 xor not t4) xor t2 and t7 xor t3 and t4 xor t5 and t1);
  t6 := ((temp shr 7) or (temp shl 25)) + ((t6 shr 11) or (t6 shl 21)) + w[23] + $F6E8DEF7;
  temp := (t7 and (t1 and t2 and t4 xor not t3) xor t1 and t6 xor t2 and t3 xor t4 and t0);
  t5 := ((temp shr 7) or (temp shl 25)) + ((t5 shr 11) or (t5 shl 21)) + w[16] + $E3FE501A;
  temp := (t6 and (t0 and t1 and t3 xor not t2) xor t0 and t5 xor t1 and t2 xor t3 and t7);
  t4 := ((temp shr 7) or (temp shl 25)) + ((t4 shr 11) or (t4 shl 21)) + w[22] + $B6794C3B;
  temp := (t5 and (t7 and t0 and t2 xor not t1) xor t7 and t4 xor t0 and t1 xor t2 and t6);
  t3 := ((temp shr 7) or (temp shl 25)) + ((t3 shr 11) or (t3 shl 21)) + w[4] + $976CE0BD;
  temp := (t4 and (t6 and t7 and t1 xor not t0) xor t6 and t3 xor t7 and t0 xor t1 and t5);
  t2 := ((temp shr 7) or (temp shl 25)) + ((t2 shr 11) or (t2 shl 21)) + w[1] + $04C006BA;
  temp := (t3 and (t5 and t6 and t0 xor not t7) xor t5 and t2 xor t6 and t7 xor t0 and t4);
  t1 := ((temp shr 7) or (temp shl 25)) + ((t1 shr 11) or (t1 shl 21)) + w[25] + $C1A94FB6;
  temp := (t2 and (t4 and t5 and t7 xor not t6) xor t4 and t1 xor t5 and t6 xor t7 and t3);
  t0 := ((temp shr 7) or (temp shl 25)) + ((t0 shr 11) or (t0 shl 21)) + w[15] + $409F60C4;

{$ENDIF}
{$ENDIF}

  Inc(CurrentHash[0], t0);
  Inc(CurrentHash[1], t1);
  Inc(CurrentHash[2], t2);
  Inc(CurrentHash[3], t3);
  Inc(CurrentHash[4], t4);
  Inc(CurrentHash[5], t5);
  Inc(CurrentHash[6], t6);
  Inc(CurrentHash[7], t7);
  FillChar(W, Sizeof(W), 0);
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure THAVAL.InitHash;
begin
  Burn;
  CurrentHash[0] := $243F6A88;
  CurrentHash[1] := $85A308D3;
  CurrentHash[2] := $13198A2E;
  CurrentHash[3] := $03707344;
  CurrentHash[4] := $A4093822;
  CurrentHash[5] := $299F31D0;
  CurrentHash[6] := $082EFA98;
  CurrentHash[7] := $EC4E6C89;
  fInitialized := true;
end;

procedure THAVAL.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure THAVAL.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi, Size shr 29);
  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure THAVAL.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

procedure THAVAL.Final(var Digest);
{$IFNDEF DIGEST256}
{$IFNDEF DIGEST224}
var
  temp              : dword;
{$ENDIF}
{$ENDIF}
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 118 then
    Compress;
{$IFDEF PASS3}
{$IFDEF DIGEST128}
  HashBuffer[118] := ((128 and 3) shl 6) or (3 shl 3) or 1;
  HashBuffer[119] := (128 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST160}
  HashBuffer[118] := ((160 and 3) shl 6) or (3 shl 3) or 1;
  HashBuffer[119] := (160 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST192}
  HashBuffer[118] := ((192 and 3) shl 6) or (3 shl 3) or 1;
  HashBuffer[119] := (192 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST224}
  HashBuffer[118] := ((224 and 3) shl 6) or (3 shl 3) or 1;
  HashBuffer[119] := (224 shr 2) and $FF;
{$ELSE}
  HashBuffer[118] := ((256 and 3) shl 6) or (3 shl 3) or 1;
  HashBuffer[119] := (256 shr 2) and $FF;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFDEF PASS4}
{$IFDEF DIGEST128}
  HashBuffer[118] := ((128 and 3) shl 6) or (4 shl 3) or 1;
  HashBuffer[119] := (128 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST160}
  HashBuffer[118] := ((160 and 3) shl 6) or (4 shl 3) or 1;
  HashBuffer[119] := (160 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST192}
  HashBuffer[118] := ((192 and 3) shl 6) or (4 shl 3) or 1;
  HashBuffer[119] := (192 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST224}
  HashBuffer[118] := ((224 and 3) shl 6) or (4 shl 3) or 1;
  HashBuffer[119] := (224 shr 2) and $FF;
{$ELSE}
  HashBuffer[118] := ((256 and 3) shl 6) or (4 shl 3) or 1;
  HashBuffer[119] := (256 shr 2) and $FF;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFDEF DIGEST128}
  HashBuffer[118] := ((128 and 3) shl 6) or (5 shl 3) or 1;
  HashBuffer[119] := (2128 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST160}
  HashBuffer[118] := ((160 and 3) shl 6) or (5 shl 3) or 1;
  HashBuffer[119] := (160 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST192}
  HashBuffer[118] := ((192 and 3) shl 6) or (5 shl 3) or 1;
  HashBuffer[119] := (192 shr 2) and $FF;
{$ELSE}
{$IFDEF DIGEST224}
  HashBuffer[118] := ((224 and 3) shl 6) or (5 shl 3) or 1;
  HashBuffer[119] := (224 shr 2) and $FF;
{$ELSE}
  HashBuffer[118] := ((256 and 3) shl 6) or (5 shl 3) or 1;
  HashBuffer[119] := (256 shr 2) and $FF;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  PDWord(@HashBuffer[120])^ := LenLo;
  PDWord(@HashBuffer[124])^ := LenHi;
  Compress;
{$IFDEF DIGEST128}
  temp := (CurrentHash[7] and $000000FF) or
    (CurrentHash[6] and $FF000000) or
    (CurrentHash[5] and $00FF0000) or
    (CurrentHash[4] and $0000FF00);
  Inc(CurrentHash[0], (temp shr 8) or (temp shl 24));
  temp := (CurrentHash[7] and $0000FF00) or
    (CurrentHash[6] and $000000FF) or
    (CurrentHash[5] and $FF000000) or
    (CurrentHash[4] and $00FF0000);
  Inc(CurrentHash[1], (temp shr 16) or (temp shl 16));
  temp := (CurrentHash[7] and $00FF0000) or
    (CurrentHash[6] and $0000FF00) or
    (CurrentHash[5] and $000000FF) or
    (CurrentHash[4] and $FF000000);
  Inc(CurrentHash[2], (temp shr 24) or (temp shl 8));
  temp := (CurrentHash[7] and $FF000000) or
    (CurrentHash[6] and $00FF0000) or
    (CurrentHash[5] and $0000FF00) or
    (CurrentHash[4] and $000000FF);
  Inc(CurrentHash[3], temp);
  Move(CurrentHash, Digest, 128 div 8);
{$ELSE}
{$IFDEF DIGEST160}
  temp := (CurrentHash[7] and $3F) or
    (CurrentHash[6] and ($7F shl 25)) or
    (CurrentHash[5] and ($3F shl 19));
  Inc(CurrentHash[0], (temp shr 19) or (temp shl 13));
  temp := (CurrentHash[7] and ($3F shl 6)) or
    (CurrentHash[6] and $3F) or
    (CurrentHash[5] and ($7F shl 25));
  Inc(CurrentHash[1], (temp shr 25) or (temp shl 7));
  temp := (CurrentHash[7] and ($7F shl 12)) or
    (CurrentHash[6] and ($3F shl 6)) or
    (CurrentHash[5] and $3F);
  Inc(CurrentHash[2], temp);
  temp := (CurrentHash[7] and ($3F shl 19)) or
    (CurrentHash[6] and ($7F shl 12)) or
    (CurrentHash[5] and ($3F shl 6));
  Inc(CurrentHash[3], temp shr 6);
  temp := (CurrentHash[7] and ($7F shl 25)) or
    (CurrentHash[6] and ($3F shl 19)) or
    (CurrentHash[5] and ($7F shl 12));
  Inc(CurrentHash[4], temp shr 12);
  Move(CurrentHash, Digest, 160 div 8);
{$ELSE}
{$IFDEF DIGEST192}
  temp := (CurrentHash[7] and $1F) or
    (CurrentHash[6] and ($3F shl 26));
  Inc(CurrentHash[0], (temp shr 26) or (temp shl 6));
  temp := (CurrentHash[7] and ($1F shl 5)) or
    (CurrentHash[6] and $1F);
  Inc(CurrentHash[1], temp);
  temp := (CurrentHash[7] and ($3F shl 10)) or
    (CurrentHash[6] and ($1F shl 5));
  Inc(CurrentHash[2], temp shr 5);
  temp := (CurrentHash[7] and ($1F shl 16)) or
    (CurrentHash[6] and ($3F shl 10));
  Inc(CurrentHash[3], temp shr 10);
  temp := (CurrentHash[7] and ($1F shl 21)) or
    (CurrentHash[6] and ($1F shl 16));
  Inc(CurrentHash[4], temp shr 16);
  temp := (CurrentHash[7] and ($3F shl 26)) or
    (CurrentHash[6] and ($1F shl 21));
  Inc(CurrentHash[5], temp shr 21);
  Move(CurrentHash, Digest, 192 div 8);
{$ELSE}
{$IFDEF DIGEST224}
  Inc(CurrentHash[0], (CurrentHash[7] shr 27) and $1F);
  Inc(CurrentHash[1], (CurrentHash[7] shr 22) and $1F);
  Inc(CurrentHash[2], (CurrentHash[7] shr 18) and $F);
  Inc(CurrentHash[3], (CurrentHash[7] shr 13) and $1F);
  Inc(CurrentHash[4], (CurrentHash[7] shr 9) and $F);
  Inc(CurrentHash[5], (CurrentHash[7] shr 4) and $1F);
  Inc(CurrentHash[6], CurrentHash[7] and $F);
  Move(CurrentHash, Digest, 224 div 8);
{$ELSE}
  Move(CurrentHash, Digest, 256 div 8);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TMD4.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewMD4;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

function LRot32(a, b: longword): longword;
begin
  Result := (a shl b) or (a shr (32 - b));
end;

procedure TMD4.Compress;
var
  Data              : array[0..15] of dword;
  A, B, C, D        : dword;
begin
  Move(HashBuffer, Data, Sizeof(Data));
  A := CurrentHash[0];
  B := CurrentHash[1];
  C := CurrentHash[2];
  D := CurrentHash[3];

  A := LRot32(A + (D xor (B and (C xor D))) + Data[0], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[1], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[2], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[3], 19);
  A := LRot32(A + (D xor (B and (C xor D))) + Data[4], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[5], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[6], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[7], 19);
  A := LRot32(A + (D xor (B and (C xor D))) + Data[8], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[9], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[10], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[11], 19);
  A := LRot32(A + (D xor (B and (C xor D))) + Data[12], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[13], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[14], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[15], 19);

  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[0] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[4] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[8] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[12] + $5A827999, 13);
  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[1] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[5] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[9] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[13] + $5A827999, 13);
  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[2] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[6] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[10] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[14] + $5A827999, 13);
  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[3] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[7] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[11] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[15] + $5A827999, 13);

  A := LRot32(A + (B xor C xor D) + Data[0] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[8] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[4] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[12] + $6ED9EBA1, 15);
  A := LRot32(A + (B xor C xor D) + Data[2] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[10] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[6] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[14] + $6ED9EBA1, 15);
  A := LRot32(A + (B xor C xor D) + Data[1] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[9] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[5] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[13] + $6ED9EBA1, 15);
  A := LRot32(A + (B xor C xor D) + Data[3] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[11] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[7] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[15] + $6ED9EBA1, 15);

  Inc(CurrentHash[0], A);
  Inc(CurrentHash[1], B);
  Inc(CurrentHash[2], C);
  Inc(CurrentHash[3], D);
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TMD4.InitHash;
begin
  Burn;
  CurrentHash[0] := $67452301;
  CurrentHash[1] := $EFCDAB89;
  CurrentHash[2] := $98BADCFE;
  CurrentHash[3] := $10325476;
  fInitialized := true;
end;

procedure TMD4.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TMD4.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi, Size shr 29);
  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TMD4.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

procedure TMD4.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 56 then
    Compress;
  PDWord(@HashBuffer[56])^ := LenLo;
  PDWord(@HashBuffer[60])^ := LenHi;
  Compress;
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TMD5.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewMD5;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TMD5.Compress;
var
  Data              : array[0..15] of dword;
  A, B, C, D        : dword;
begin
  Move(HashBuffer, Data, Sizeof(Data));
  A := CurrentHash[0];
  B := CurrentHash[1];
  C := CurrentHash[2];
  D := CurrentHash[3];

  A := B + LRot32(A + (D xor (B and (C xor D))) + Data[0] + $D76AA478, 7);
  D := A + LRot32(D + (C xor (A and (B xor C))) + Data[1] + $E8C7B756, 12);
  C := D + LRot32(C + (B xor (D and (A xor B))) + Data[2] + $242070DB, 17);
  B := C + LRot32(B + (A xor (C and (D xor A))) + Data[3] + $C1BDCEEE, 22);
  A := B + LRot32(A + (D xor (B and (C xor D))) + Data[4] + $F57C0FAF, 7);
  D := A + LRot32(D + (C xor (A and (B xor C))) + Data[5] + $4787C62A, 12);
  C := D + LRot32(C + (B xor (D and (A xor B))) + Data[6] + $A8304613, 17);
  B := C + LRot32(B + (A xor (C and (D xor A))) + Data[7] + $FD469501, 22);
  A := B + LRot32(A + (D xor (B and (C xor D))) + Data[8] + $698098D8, 7);
  D := A + LRot32(D + (C xor (A and (B xor C))) + Data[9] + $8B44F7AF, 12);
  C := D + LRot32(C + (B xor (D and (A xor B))) + Data[10] + $FFFF5BB1, 17);
  B := C + LRot32(B + (A xor (C and (D xor A))) + Data[11] + $895CD7BE, 22);
  A := B + LRot32(A + (D xor (B and (C xor D))) + Data[12] + $6B901122, 7);
  D := A + LRot32(D + (C xor (A and (B xor C))) + Data[13] + $FD987193, 12);
  C := D + LRot32(C + (B xor (D and (A xor B))) + Data[14] + $A679438E, 17);
  B := C + LRot32(B + (A xor (C and (D xor A))) + Data[15] + $49B40821, 22);

  A := B + LRot32(A + (C xor (D and (B xor C))) + Data[1] + $F61E2562, 5);
  D := A + LRot32(D + (B xor (C and (A xor B))) + Data[6] + $C040B340, 9);
  C := D + LRot32(C + (A xor (B and (D xor A))) + Data[11] + $265E5A51, 14);
  B := C + LRot32(B + (D xor (A and (C xor D))) + Data[0] + $E9B6C7AA, 20);
  A := B + LRot32(A + (C xor (D and (B xor C))) + Data[5] + $D62F105D, 5);
  D := A + LRot32(D + (B xor (C and (A xor B))) + Data[10] + $02441453, 9);
  C := D + LRot32(C + (A xor (B and (D xor A))) + Data[15] + $D8A1E681, 14);
  B := C + LRot32(B + (D xor (A and (C xor D))) + Data[4] + $E7D3FBC8, 20);
  A := B + LRot32(A + (C xor (D and (B xor C))) + Data[9] + $21E1CDE6, 5);
  D := A + LRot32(D + (B xor (C and (A xor B))) + Data[14] + $C33707D6, 9);
  C := D + LRot32(C + (A xor (B and (D xor A))) + Data[3] + $F4D50D87, 14);
  B := C + LRot32(B + (D xor (A and (C xor D))) + Data[8] + $455A14ED, 20);
  A := B + LRot32(A + (C xor (D and (B xor C))) + Data[13] + $A9E3E905, 5);
  D := A + LRot32(D + (B xor (C and (A xor B))) + Data[2] + $FCEFA3F8, 9);
  C := D + LRot32(C + (A xor (B and (D xor A))) + Data[7] + $676F02D9, 14);
  B := C + LRot32(B + (D xor (A and (C xor D))) + Data[12] + $8D2A4C8A, 20);

  A := B + LRot32(A + (B xor C xor D) + Data[5] + $FFFA3942, 4);
  D := A + LRot32(D + (A xor B xor C) + Data[8] + $8771F681, 11);
  C := D + LRot32(C + (D xor A xor B) + Data[11] + $6D9D6122, 16);
  B := C + LRot32(B + (C xor D xor A) + Data[14] + $FDE5380C, 23);
  A := B + LRot32(A + (B xor C xor D) + Data[1] + $A4BEEA44, 4);
  D := A + LRot32(D + (A xor B xor C) + Data[4] + $4BDECFA9, 11);
  C := D + LRot32(C + (D xor A xor B) + Data[7] + $F6BB4B60, 16);
  B := C + LRot32(B + (C xor D xor A) + Data[10] + $BEBFBC70, 23);
  A := B + LRot32(A + (B xor C xor D) + Data[13] + $289B7EC6, 4);
  D := A + LRot32(D + (A xor B xor C) + Data[0] + $EAA127FA, 11);
  C := D + LRot32(C + (D xor A xor B) + Data[3] + $D4EF3085, 16);
  B := C + LRot32(B + (C xor D xor A) + Data[6] + $04881D05, 23);
  A := B + LRot32(A + (B xor C xor D) + Data[9] + $D9D4D039, 4);
  D := A + LRot32(D + (A xor B xor C) + Data[12] + $E6DB99E5, 11);
  C := D + LRot32(C + (D xor A xor B) + Data[15] + $1FA27CF8, 16);
  B := C + LRot32(B + (C xor D xor A) + Data[2] + $C4AC5665, 23);

  A := B + LRot32(A + (C xor (B or (not D))) + Data[0] + $F4292244, 6);
  D := A + LRot32(D + (B xor (A or (not C))) + Data[7] + $432AFF97, 10);
  C := D + LRot32(C + (A xor (D or (not B))) + Data[14] + $AB9423A7, 15);
  B := C + LRot32(B + (D xor (C or (not A))) + Data[5] + $FC93A039, 21);
  A := B + LRot32(A + (C xor (B or (not D))) + Data[12] + $655B59C3, 6);
  D := A + LRot32(D + (B xor (A or (not C))) + Data[3] + $8F0CCC92, 10);
  C := D + LRot32(C + (A xor (D or (not B))) + Data[10] + $FFEFF47D, 15);
  B := C + LRot32(B + (D xor (C or (not A))) + Data[1] + $85845DD1, 21);
  A := B + LRot32(A + (C xor (B or (not D))) + Data[8] + $6FA87E4F, 6);
  D := A + LRot32(D + (B xor (A or (not C))) + Data[15] + $FE2CE6E0, 10);
  C := D + LRot32(C + (A xor (D or (not B))) + Data[6] + $A3014314, 15);
  B := C + LRot32(B + (D xor (C or (not A))) + Data[13] + $4E0811A1, 21);
  A := B + LRot32(A + (C xor (B or (not D))) + Data[4] + $F7537E82, 6);
  D := A + LRot32(D + (B xor (A or (not C))) + Data[11] + $BD3AF235, 10);
  C := D + LRot32(C + (A xor (D or (not B))) + Data[2] + $2AD7D2BB, 15);
  B := C + LRot32(B + (D xor (C or (not A))) + Data[9] + $EB86D391, 21);

  Inc(CurrentHash[0], A);
  Inc(CurrentHash[1], B);
  Inc(CurrentHash[2], C);
  Inc(CurrentHash[3], D);
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TMD5.InitHash;
begin
  Burn;
  CurrentHash[0] := $67452301;
  CurrentHash[1] := $EFCDAB89;
  CurrentHash[2] := $98BADCFE;
  CurrentHash[3] := $10325476;
  fInitialized := true;
end;

procedure TMD5.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TMD5.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi, Size shr 29);
  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TMD5.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

procedure TMD5.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 56 then
    Compress;
  PDWord(@HashBuffer[56])^ := LenLo;
  PDWord(@HashBuffer[60])^ := LenHi;
  Compress;
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TRMD128.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewRMD128;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TRMD128.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

{$R-}{$Q-}

procedure TRMD128.Compress;
var
  X                 : array[0..15] of DWord;
  a, aa, b, bb, c, cc, d, dd, t: dword;
begin
  Move(HashBuffer, X, Sizeof(X));
  a := CurrentHash[0];
  aa := a;
  b := CurrentHash[1];
  bb := b;
  c := CurrentHash[2];
  cc := c;
  d := CurrentHash[3];
  dd := d;

  t := a + (b xor c xor d) + X[0];
  a := (t shl 11) or (t shr (32 - 11));
  t := d + (a xor b xor c) + X[1];
  d := (t shl 14) or (t shr (32 - 14));
  t := c + (d xor a xor b) + X[2];
  c := (t shl 15) or (t shr (32 - 15));
  t := b + (c xor d xor a) + X[3];
  b := (t shl 12) or (t shr (32 - 12));
  t := a + (b xor c xor d) + X[4];
  a := (t shl 5) or (t shr (32 - 5));
  t := d + (a xor b xor c) + X[5];
  d := (t shl 8) or (t shr (32 - 8));
  t := c + (d xor a xor b) + X[6];
  c := (t shl 7) or (t shr (32 - 7));
  t := b + (c xor d xor a) + X[7];
  b := (t shl 9) or (t shr (32 - 9));
  t := a + (b xor c xor d) + X[8];
  a := (t shl 11) or (t shr (32 - 11));
  t := d + (a xor b xor c) + X[9];
  d := (t shl 13) or (t shr (32 - 13));
  t := c + (d xor a xor b) + X[10];
  c := (t shl 14) or (t shr (32 - 14));
  t := b + (c xor d xor a) + X[11];
  b := (t shl 15) or (t shr (32 - 15));
  t := a + (b xor c xor d) + X[12];
  a := (t shl 6) or (t shr (32 - 6));
  t := d + (a xor b xor c) + X[13];
  d := (t shl 7) or (t shr (32 - 7));
  t := c + (d xor a xor b) + X[14];
  c := (t shl 9) or (t shr (32 - 9));
  t := b + (c xor d xor a) + X[15];
  b := (t shl 8) or (t shr (32 - 8));

  t := a + ((b and c) or (not b and d)) + X[7] + $5A827999;
  a := (t shl 7) or (t shr (32 - 7));
  t := d + ((a and b) or (not a and c)) + X[4] + $5A827999;
  d := (t shl 6) or (t shr (32 - 6));
  t := c + ((d and a) or (not d and b)) + X[13] + $5A827999;
  c := (t shl 8) or (t shr (32 - 8));
  t := b + ((c and d) or (not c and a)) + X[1] + $5A827999;
  b := (t shl 13) or (t shr (32 - 13));
  t := a + ((b and c) or (not b and d)) + X[10] + $5A827999;
  a := (t shl 11) or (t shr (32 - 11));
  t := d + ((a and b) or (not a and c)) + X[6] + $5A827999;
  d := (t shl 9) or (t shr (32 - 9));
  t := c + ((d and a) or (not d and b)) + X[15] + $5A827999;
  c := (t shl 7) or (t shr (32 - 7));
  t := b + ((c and d) or (not c and a)) + X[3] + $5A827999;
  b := (t shl 15) or (t shr (32 - 15));
  t := a + ((b and c) or (not b and d)) + X[12] + $5A827999;
  a := (t shl 7) or (t shr (32 - 7));
  t := d + ((a and b) or (not a and c)) + X[0] + $5A827999;
  d := (t shl 12) or (t shr (32 - 12));
  t := c + ((d and a) or (not d and b)) + X[9] + $5A827999;
  c := (t shl 15) or (t shr (32 - 15));
  t := b + ((c and d) or (not c and a)) + X[5] + $5A827999;
  b := (t shl 9) or (t shr (32 - 9));
  t := a + ((b and c) or (not b and d)) + X[2] + $5A827999;
  a := (t shl 11) or (t shr (32 - 11));
  t := d + ((a and b) or (not a and c)) + X[14] + $5A827999;
  d := (t shl 7) or (t shr (32 - 7));
  t := c + ((d and a) or (not d and b)) + X[11] + $5A827999;
  c := (t shl 13) or (t shr (32 - 13));
  t := b + ((c and d) or (not c and a)) + X[8] + $5A827999;
  b := (t shl 12) or (t shr (32 - 12));

  t := a + ((b or not c) xor d) + X[3] + $6ED9EBA1;
  a := (t shl 11) or (t shr (32 - 11));
  t := d + ((a or not b) xor c) + X[10] + $6ED9EBA1;
  d := (t shl 13) or (t shr (32 - 13));
  t := c + ((d or not a) xor b) + X[14] + $6ED9EBA1;
  c := (t shl 6) or (t shr (32 - 6));
  t := b + ((c or not d) xor a) + X[4] + $6ED9EBA1;
  b := (t shl 7) or (t shr (32 - 7));
  t := a + ((b or not c) xor d) + X[9] + $6ED9EBA1;
  a := (t shl 14) or (t shr (32 - 14));
  t := d + ((a or not b) xor c) + X[15] + $6ED9EBA1;
  d := (t shl 9) or (t shr (32 - 9));
  t := c + ((d or not a) xor b) + X[8] + $6ED9EBA1;
  c := (t shl 13) or (t shr (32 - 13));
  t := b + ((c or not d) xor a) + X[1] + $6ED9EBA1;
  b := (t shl 15) or (t shr (32 - 15));
  t := a + ((b or not c) xor d) + X[2] + $6ED9EBA1;
  a := (t shl 14) or (t shr (32 - 14));
  t := d + ((a or not b) xor c) + X[7] + $6ED9EBA1;
  d := (t shl 8) or (t shr (32 - 8));
  t := c + ((d or not a) xor b) + X[0] + $6ED9EBA1;
  c := (t shl 13) or (t shr (32 - 13));
  t := b + ((c or not d) xor a) + X[6] + $6ED9EBA1;
  b := (t shl 6) or (t shr (32 - 6));
  t := a + ((b or not c) xor d) + X[13] + $6ED9EBA1;
  a := (t shl 5) or (t shr (32 - 5));
  t := d + ((a or not b) xor c) + X[11] + $6ED9EBA1;
  d := (t shl 12) or (t shr (32 - 12));
  t := c + ((d or not a) xor b) + X[5] + $6ED9EBA1;
  c := (t shl 7) or (t shr (32 - 7));
  t := b + ((c or not d) xor a) + X[12] + $6ED9EBA1;
  b := (t shl 5) or (t shr (32 - 5));

  t := a + ((b and d) or (c and not d)) + X[1] + $8F1BBCDC;
  a := (t shl 11) or (t shr (32 - 11));
  t := d + ((a and c) or (b and not c)) + X[9] + $8F1BBCDC;
  d := (t shl 12) or (t shr (32 - 12));
  t := c + ((d and b) or (a and not b)) + X[11] + $8F1BBCDC;
  c := (t shl 14) or (t shr (32 - 14));
  t := b + ((c and a) or (d and not a)) + X[10] + $8F1BBCDC;
  b := (t shl 15) or (t shr (32 - 15));
  t := a + ((b and d) or (c and not d)) + X[0] + $8F1BBCDC;
  a := (t shl 14) or (t shr (32 - 14));
  t := d + ((a and c) or (b and not c)) + X[8] + $8F1BBCDC;
  d := (t shl 15) or (t shr (32 - 15));
  t := c + ((d and b) or (a and not b)) + X[12] + $8F1BBCDC;
  c := (t shl 9) or (t shr (32 - 9));
  t := b + ((c and a) or (d and not a)) + X[4] + $8F1BBCDC;
  b := (t shl 8) or (t shr (32 - 8));
  t := a + ((b and d) or (c and not d)) + X[13] + $8F1BBCDC;
  a := (t shl 9) or (t shr (32 - 9));
  t := d + ((a and c) or (b and not c)) + X[3] + $8F1BBCDC;
  d := (t shl 14) or (t shr (32 - 14));
  t := c + ((d and b) or (a and not b)) + X[7] + $8F1BBCDC;
  c := (t shl 5) or (t shr (32 - 5));
  t := b + ((c and a) or (d and not a)) + X[15] + $8F1BBCDC;
  b := (t shl 6) or (t shr (32 - 6));
  t := a + ((b and d) or (c and not d)) + X[14] + $8F1BBCDC;
  a := (t shl 8) or (t shr (32 - 8));
  t := d + ((a and c) or (b and not c)) + X[5] + $8F1BBCDC;
  d := (t shl 6) or (t shr (32 - 6));
  t := c + ((d and b) or (a and not b)) + X[6] + $8F1BBCDC;
  c := (t shl 5) or (t shr (32 - 5));
  t := b + ((c and a) or (d and not a)) + X[2] + $8F1BBCDC;
  b := (t shl 12) or (t shr (32 - 12));

  t := aa + ((bb and dd) or (cc and not dd)) + X[5] + $50A28BE6;
  aa := (t shl 8) or (t shr (32 - 8));
  t := dd + ((aa and cc) or (bb and not cc)) + X[14] + $50A28BE6;
  dd := (t shl 9) or (t shr (32 - 9));
  t := cc + ((dd and bb) or (aa and not bb)) + X[7] + $50A28BE6;
  cc := (t shl 9) or (t shr (32 - 9));
  t := bb + ((cc and aa) or (dd and not aa)) + X[0] + $50A28BE6;
  bb := (t shl 11) or (t shr (32 - 11));
  t := aa + ((bb and dd) or (cc and not dd)) + X[9] + $50A28BE6;
  aa := (t shl 13) or (t shr (32 - 13));
  t := dd + ((aa and cc) or (bb and not cc)) + X[2] + $50A28BE6;
  dd := (t shl 15) or (t shr (32 - 15));
  t := cc + ((dd and bb) or (aa and not bb)) + X[11] + $50A28BE6;
  cc := (t shl 15) or (t shr (32 - 15));
  t := bb + ((cc and aa) or (dd and not aa)) + X[4] + $50A28BE6;
  bb := (t shl 5) or (t shr (32 - 5));
  t := aa + ((bb and dd) or (cc and not dd)) + X[13] + $50A28BE6;
  aa := (t shl 7) or (t shr (32 - 7));
  t := dd + ((aa and cc) or (bb and not cc)) + X[6] + $50A28BE6;
  dd := (t shl 7) or (t shr (32 - 7));
  t := cc + ((dd and bb) or (aa and not bb)) + X[15] + $50A28BE6;
  cc := (t shl 8) or (t shr (32 - 8));
  t := bb + ((cc and aa) or (dd and not aa)) + X[8] + $50A28BE6;
  bb := (t shl 11) or (t shr (32 - 11));
  t := aa + ((bb and dd) or (cc and not dd)) + X[1] + $50A28BE6;
  aa := (t shl 14) or (t shr (32 - 14));
  t := dd + ((aa and cc) or (bb and not cc)) + X[10] + $50A28BE6;
  dd := (t shl 14) or (t shr (32 - 14));
  t := cc + ((dd and bb) or (aa and not bb)) + X[3] + $50A28BE6;
  cc := (t shl 12) or (t shr (32 - 12));
  t := bb + ((cc and aa) or (dd and not aa)) + X[12] + $50A28BE6;
  bb := (t shl 6) or (t shr (32 - 6));

  t := aa + ((bb or not cc) xor dd) + X[6] + $5C4DD124;
  aa := (t shl 9) or (t shr (32 - 9));
  t := dd + ((aa or not bb) xor cc) + X[11] + $5C4DD124;
  dd := (t shl 13) or (t shr (32 - 13));
  t := cc + ((dd or not aa) xor bb) + X[3] + $5C4DD124;
  cc := (t shl 15) or (t shr (32 - 15));
  t := bb + ((cc or not dd) xor aa) + X[7] + $5C4DD124;
  bb := (t shl 7) or (t shr (32 - 7));
  t := aa + ((bb or not cc) xor dd) + X[0] + $5C4DD124;
  aa := (t shl 12) or (t shr (32 - 12));
  t := dd + ((aa or not bb) xor cc) + X[13] + $5C4DD124;
  dd := (t shl 8) or (t shr (32 - 8));
  t := cc + ((dd or not aa) xor bb) + X[5] + $5C4DD124;
  cc := (t shl 9) or (t shr (32 - 9));
  t := bb + ((cc or not dd) xor aa) + X[10] + $5C4DD124;
  bb := (t shl 11) or (t shr (32 - 11));
  t := aa + ((bb or not cc) xor dd) + X[14] + $5C4DD124;
  aa := (t shl 7) or (t shr (32 - 7));
  t := dd + ((aa or not bb) xor cc) + X[15] + $5C4DD124;
  dd := (t shl 7) or (t shr (32 - 7));
  t := cc + ((dd or not aa) xor bb) + X[8] + $5C4DD124;
  cc := (t shl 12) or (t shr (32 - 12));
  t := bb + ((cc or not dd) xor aa) + X[12] + $5C4DD124;
  bb := (t shl 7) or (t shr (32 - 7));
  t := aa + ((bb or not cc) xor dd) + X[4] + $5C4DD124;
  aa := (t shl 6) or (t shr (32 - 6));
  t := dd + ((aa or not bb) xor cc) + X[9] + $5C4DD124;
  dd := (t shl 15) or (t shr (32 - 15));
  t := cc + ((dd or not aa) xor bb) + X[1] + $5C4DD124;
  cc := (t shl 13) or (t shr (32 - 13));
  t := bb + ((cc or not dd) xor aa) + X[2] + $5C4DD124;
  bb := (t shl 11) or (t shr (32 - 11));

  t := aa + ((bb and cc) or (not bb and dd)) + X[15] + $6D703EF3;
  aa := (t shl 9) or (t shr (32 - 9));
  t := dd + ((aa and bb) or (not aa and cc)) + X[5] + $6D703EF3;
  dd := (t shl 7) or (t shr (32 - 7));
  t := cc + ((dd and aa) or (not dd and bb)) + X[1] + $6D703EF3;
  cc := (t shl 15) or (t shr (32 - 15));
  t := bb + ((cc and dd) or (not cc and aa)) + X[3] + $6D703EF3;
  bb := (t shl 11) or (t shr (32 - 11));
  t := aa + ((bb and cc) or (not bb and dd)) + X[7] + $6D703EF3;
  aa := (t shl 8) or (t shr (32 - 8));
  t := dd + ((aa and bb) or (not aa and cc)) + X[14] + $6D703EF3;
  dd := (t shl 6) or (t shr (32 - 6));
  t := cc + ((dd and aa) or (not dd and bb)) + X[6] + $6D703EF3;
  cc := (t shl 6) or (t shr (32 - 6));
  t := bb + ((cc and dd) or (not cc and aa)) + X[9] + $6D703EF3;
  bb := (t shl 14) or (t shr (32 - 14));
  t := aa + ((bb and cc) or (not bb and dd)) + X[11] + $6D703EF3;
  aa := (t shl 12) or (t shr (32 - 12));
  t := dd + ((aa and bb) or (not aa and cc)) + X[8] + $6D703EF3;
  dd := (t shl 13) or (t shr (32 - 13));
  t := cc + ((dd and aa) or (not dd and bb)) + X[12] + $6D703EF3;
  cc := (t shl 5) or (t shr (32 - 5));
  t := bb + ((cc and dd) or (not cc and aa)) + X[2] + $6D703EF3;
  bb := (t shl 14) or (t shr (32 - 14));
  t := aa + ((bb and cc) or (not bb and dd)) + X[10] + $6D703EF3;
  aa := (t shl 13) or (t shr (32 - 13));
  t := dd + ((aa and bb) or (not aa and cc)) + X[0] + $6D703EF3;
  dd := (t shl 13) or (t shr (32 - 13));
  t := cc + ((dd and aa) or (not dd and bb)) + X[4] + $6D703EF3;
  cc := (t shl 7) or (t shr (32 - 7));
  t := bb + ((cc and dd) or (not cc and aa)) + X[13] + $6D703EF3;
  bb := (t shl 5) or (t shr (32 - 5));

  t := aa + (bb xor cc xor dd) + X[8];
  aa := (t shl 15) or (t shr (32 - 15));
  t := dd + (aa xor bb xor cc) + X[6];
  dd := (t shl 5) or (t shr (32 - 5));
  t := cc + (dd xor aa xor bb) + X[4];
  cc := (t shl 8) or (t shr (32 - 8));
  t := bb + (cc xor dd xor aa) + X[1];
  bb := (t shl 11) or (t shr (32 - 11));
  t := aa + (bb xor cc xor dd) + X[3];
  aa := (t shl 14) or (t shr (32 - 14));
  t := dd + (aa xor bb xor cc) + X[11];
  dd := (t shl 14) or (t shr (32 - 14));
  t := cc + (dd xor aa xor bb) + X[15];
  cc := (t shl 6) or (t shr (32 - 6));
  t := bb + (cc xor dd xor aa) + X[0];
  bb := (t shl 14) or (t shr (32 - 14));
  t := aa + (bb xor cc xor dd) + X[5];
  aa := (t shl 6) or (t shr (32 - 6));
  t := dd + (aa xor bb xor cc) + X[12];
  dd := (t shl 9) or (t shr (32 - 9));
  t := cc + (dd xor aa xor bb) + X[2];
  cc := (t shl 12) or (t shr (32 - 12));
  t := bb + (cc xor dd xor aa) + X[13];
  bb := (t shl 9) or (t shr (32 - 9));
  t := aa + (bb xor cc xor dd) + X[9];
  aa := (t shl 12) or (t shr (32 - 12));
  t := dd + (aa xor bb xor cc) + X[7];
  dd := (t shl 5) or (t shr (32 - 5));
  t := cc + (dd xor aa xor bb) + X[10];
  cc := (t shl 15) or (t shr (32 - 15));
  t := bb + (cc xor dd xor aa) + X[14];
  bb := (t shl 8) or (t shr (32 - 8));

  Inc(dd, c + CurrentHash[1]);
  CurrentHash[1] := CurrentHash[2] + d + aa;
  CurrentHash[2] := CurrentHash[3] + a + bb;
  CurrentHash[3] := CurrentHash[0] + b + cc;
  CurrentHash[0] := dd;

  FillChar(X, Sizeof(X), 0);
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TRMD128.InitHash;
begin
  Burn;
  CurrentHash[0] := $67452301;
  CurrentHash[1] := $EFCDAB89;
  CurrentHash[2] := $98BADCFE;
  CurrentHash[3] := $10325476;
  fInitialized := true;
end;

procedure TRMD128.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TRMD128.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi, Size shr 29);
  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TRMD128.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 56 then
    Compress;
  PDWord(@HashBuffer[56])^ := LenLo;
  PDWord(@HashBuffer[60])^ := LenHi;
  Compress;
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TRMD160.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewRMD160;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TRMD160.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

{$R-}{$Q-}

procedure TRMD160.Compress;
var
  aa, bb, cc, dd, ee, aaa, bbb, ccc, ddd, eee: DWord;
  X                 : array[0..15] of DWord;
begin
  Move(HashBuffer, X, Sizeof(X));
  aa := CurrentHash[0];
  aaa := CurrentHash[0];
  bb := CurrentHash[1];
  bbb := CurrentHash[1];
  cc := CurrentHash[2];
  ccc := CurrentHash[2];
  dd := CurrentHash[3];
  ddd := CurrentHash[3];
  ee := CurrentHash[4];
  eee := CurrentHash[4];

  aa := aa + (bb xor cc xor dd) + X[0];
  aa := ((aa shl 11) or (aa shr (32 - 11))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + (aa xor bb xor cc) + X[1];
  ee := ((ee shl 14) or (ee shr (32 - 14))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + (ee xor aa xor bb) + X[2];
  dd := ((dd shl 15) or (dd shr (32 - 15))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + (dd xor ee xor aa) + X[3];
  cc := ((cc shl 12) or (cc shr (32 - 12))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + (cc xor dd xor ee) + X[4];
  bb := ((bb shl 5) or (bb shr (32 - 5))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + (bb xor cc xor dd) + X[5];
  aa := ((aa shl 8) or (aa shr (32 - 8))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + (aa xor bb xor cc) + X[6];
  ee := ((ee shl 7) or (ee shr (32 - 7))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + (ee xor aa xor bb) + X[7];
  dd := ((dd shl 9) or (dd shr (32 - 9))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + (dd xor ee xor aa) + X[8];
  cc := ((cc shl 11) or (cc shr (32 - 11))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + (cc xor dd xor ee) + X[9];
  bb := ((bb shl 13) or (bb shr (32 - 13))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + (bb xor cc xor dd) + X[10];
  aa := ((aa shl 14) or (aa shr (32 - 14))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + (aa xor bb xor cc) + X[11];
  ee := ((ee shl 15) or (ee shr (32 - 15))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + (ee xor aa xor bb) + X[12];
  dd := ((dd shl 6) or (dd shr (32 - 6))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + (dd xor ee xor aa) + X[13];
  cc := ((cc shl 7) or (cc shr (32 - 7))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + (cc xor dd xor ee) + X[14];
  bb := ((bb shl 9) or (bb shr (32 - 9))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + (bb xor cc xor dd) + X[15];
  aa := ((aa shl 8) or (aa shr (32 - 8))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));

  ee := ee + ((aa and bb) or ((not aa) and cc)) + X[7] + $5A827999;
  ee := ((ee shl 7) or (ee shr (32 - 7))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee and aa) or ((not ee) and bb)) + X[4] + $5A827999;
  dd := ((dd shl 6) or (dd shr (32 - 6))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd and ee) or ((not dd) and aa)) + X[13] + $5A827999;
  cc := ((cc shl 8) or (cc shr (32 - 8))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc and dd) or ((not cc) and ee)) + X[1] + $5A827999;
  bb := ((bb shl 13) or (bb shr (32 - 13))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb and cc) or ((not bb) and dd)) + X[10] + $5A827999;
  aa := ((aa shl 11) or (aa shr (32 - 11))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa and bb) or ((not aa) and cc)) + X[6] + $5A827999;
  ee := ((ee shl 9) or (ee shr (32 - 9))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee and aa) or ((not ee) and bb)) + X[15] + $5A827999;
  dd := ((dd shl 7) or (dd shr (32 - 7))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd and ee) or ((not dd) and aa)) + X[3] + $5A827999;
  cc := ((cc shl 15) or (cc shr (32 - 15))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc and dd) or ((not cc) and ee)) + X[12] + $5A827999;
  bb := ((bb shl 7) or (bb shr (32 - 7))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb and cc) or ((not bb) and dd)) + X[0] + $5A827999;
  aa := ((aa shl 12) or (aa shr (32 - 12))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa and bb) or ((not aa) and cc)) + X[9] + $5A827999;
  ee := ((ee shl 15) or (ee shr (32 - 15))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee and aa) or ((not ee) and bb)) + X[5] + $5A827999;
  dd := ((dd shl 9) or (dd shr (32 - 9))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd and ee) or ((not dd) and aa)) + X[2] + $5A827999;
  cc := ((cc shl 11) or (cc shr (32 - 11))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc and dd) or ((not cc) and ee)) + X[14] + $5A827999;
  bb := ((bb shl 7) or (bb shr (32 - 7))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb and cc) or ((not bb) and dd)) + X[11] + $5A827999;
  aa := ((aa shl 13) or (aa shr (32 - 13))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa and bb) or ((not aa) and cc)) + X[8] + $5A827999;
  ee := ((ee shl 12) or (ee shr (32 - 12))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));

  dd := dd + ((ee or (not aa)) xor bb) + X[3] + $6ED9EBA1;
  dd := ((dd shl 11) or (dd shr (32 - 11))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd or (not ee)) xor aa) + X[10] + $6ED9EBA1;
  cc := ((cc shl 13) or (cc shr (32 - 13))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc or (not dd)) xor ee) + X[14] + $6ED9EBA1;
  bb := ((bb shl 6) or (bb shr (32 - 6))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb or (not cc)) xor dd) + X[4] + $6ED9EBA1;
  aa := ((aa shl 7) or (aa shr (32 - 7))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa or (not bb)) xor cc) + X[9] + $6ED9EBA1;
  ee := ((ee shl 14) or (ee shr (32 - 14))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee or (not aa)) xor bb) + X[15] + $6ED9EBA1;
  dd := ((dd shl 9) or (dd shr (32 - 9))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd or (not ee)) xor aa) + X[8] + $6ED9EBA1;
  cc := ((cc shl 13) or (cc shr (32 - 13))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc or (not dd)) xor ee) + X[1] + $6ED9EBA1;
  bb := ((bb shl 15) or (bb shr (32 - 15))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb or (not cc)) xor dd) + X[2] + $6ED9EBA1;
  aa := ((aa shl 14) or (aa shr (32 - 14))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa or (not bb)) xor cc) + X[7] + $6ED9EBA1;
  ee := ((ee shl 8) or (ee shr (32 - 8))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee or (not aa)) xor bb) + X[0] + $6ED9EBA1;
  dd := ((dd shl 13) or (dd shr (32 - 13))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd or (not ee)) xor aa) + X[6] + $6ED9EBA1;
  cc := ((cc shl 6) or (cc shr (32 - 6))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc or (not dd)) xor ee) + X[13] + $6ED9EBA1;
  bb := ((bb shl 5) or (bb shr (32 - 5))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb or (not cc)) xor dd) + X[11] + $6ED9EBA1;
  aa := ((aa shl 12) or (aa shr (32 - 12))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa or (not bb)) xor cc) + X[5] + $6ED9EBA1;
  ee := ((ee shl 7) or (ee shr (32 - 7))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee or (not aa)) xor bb) + X[12] + $6ED9EBA1;
  dd := ((dd shl 5) or (dd shr (32 - 5))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));

  cc := cc + ((dd and aa) or (ee and (not aa))) + X[1] + $8F1BBCDC;
  cc := ((cc shl 11) or (cc shr (32 - 11))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc and ee) or (dd and (not ee))) + X[9] + $8F1BBCDC;
  bb := ((bb shl 12) or (bb shr (32 - 12))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb and dd) or (cc and (not dd))) + X[11] + $8F1BBCDC;
  aa := ((aa shl 14) or (aa shr (32 - 14))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa and cc) or (bb and (not cc))) + X[10] + $8F1BBCDC;
  ee := ((ee shl 15) or (ee shr (32 - 15))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee and bb) or (aa and (not bb))) + X[0] + $8F1BBCDC;
  dd := ((dd shl 14) or (dd shr (32 - 14))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd and aa) or (ee and (not aa))) + X[8] + $8F1BBCDC;
  cc := ((cc shl 15) or (cc shr (32 - 15))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc and ee) or (dd and (not ee))) + X[12] + $8F1BBCDC;
  bb := ((bb shl 9) or (bb shr (32 - 9))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb and dd) or (cc and (not dd))) + X[4] + $8F1BBCDC;
  aa := ((aa shl 8) or (aa shr (32 - 8))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa and cc) or (bb and (not cc))) + X[13] + $8F1BBCDC;
  ee := ((ee shl 9) or (ee shr (32 - 9))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee and bb) or (aa and (not bb))) + X[3] + $8F1BBCDC;
  dd := ((dd shl 14) or (dd shr (32 - 14))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd and aa) or (ee and (not aa))) + X[7] + $8F1BBCDC;
  cc := ((cc shl 5) or (cc shr (32 - 5))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + ((cc and ee) or (dd and (not ee))) + X[15] + $8F1BBCDC;
  bb := ((bb shl 6) or (bb shr (32 - 6))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + ((bb and dd) or (cc and (not dd))) + X[14] + $8F1BBCDC;
  aa := ((aa shl 8) or (aa shr (32 - 8))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + ((aa and cc) or (bb and (not cc))) + X[5] + $8F1BBCDC;
  ee := ((ee shl 6) or (ee shr (32 - 6))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + ((ee and bb) or (aa and (not bb))) + X[6] + $8F1BBCDC;
  dd := ((dd shl 5) or (dd shr (32 - 5))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + ((dd and aa) or (ee and (not aa))) + X[2] + $8F1BBCDC;
  cc := ((cc shl 12) or (cc shr (32 - 12))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));

  bb := bb + (cc xor (dd or (not ee))) + X[4] + $A953FD4E;
  bb := ((bb shl 9) or (bb shr (32 - 9))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + (bb xor (cc or (not dd))) + X[0] + $A953FD4E;
  aa := ((aa shl 15) or (aa shr (32 - 15))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + (aa xor (bb or (not cc))) + X[5] + $A953FD4E;
  ee := ((ee shl 5) or (ee shr (32 - 5))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + (ee xor (aa or (not bb))) + X[9] + $A953FD4E;
  dd := ((dd shl 11) or (dd shr (32 - 11))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + (dd xor (ee or (not aa))) + X[7] + $A953FD4E;
  cc := ((cc shl 6) or (cc shr (32 - 6))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + (cc xor (dd or (not ee))) + X[12] + $A953FD4E;
  bb := ((bb shl 8) or (bb shr (32 - 8))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + (bb xor (cc or (not dd))) + X[2] + $A953FD4E;
  aa := ((aa shl 13) or (aa shr (32 - 13))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + (aa xor (bb or (not cc))) + X[10] + $A953FD4E;
  ee := ((ee shl 12) or (ee shr (32 - 12))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + (ee xor (aa or (not bb))) + X[14] + $A953FD4E;
  dd := ((dd shl 5) or (dd shr (32 - 5))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + (dd xor (ee or (not aa))) + X[1] + $A953FD4E;
  cc := ((cc shl 12) or (cc shr (32 - 12))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + (cc xor (dd or (not ee))) + X[3] + $A953FD4E;
  bb := ((bb shl 13) or (bb shr (32 - 13))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));
  aa := aa + (bb xor (cc or (not dd))) + X[8] + $A953FD4E;
  aa := ((aa shl 14) or (aa shr (32 - 14))) + ee;
  cc := ((cc shl 10) or (cc shr (32 - 10)));
  ee := ee + (aa xor (bb or (not cc))) + X[11] + $A953FD4E;
  ee := ((ee shl 11) or (ee shr (32 - 11))) + dd;
  bb := ((bb shl 10) or (bb shr (32 - 10)));
  dd := dd + (ee xor (aa or (not bb))) + X[6] + $A953FD4E;
  dd := ((dd shl 8) or (dd shr (32 - 8))) + cc;
  aa := ((aa shl 10) or (aa shr (32 - 10)));
  cc := cc + (dd xor (ee or (not aa))) + X[15] + $A953FD4E;
  cc := ((cc shl 5) or (cc shr (32 - 5))) + bb;
  ee := ((ee shl 10) or (ee shr (32 - 10)));
  bb := bb + (cc xor (dd or (not ee))) + X[13] + $A953FD4E;
  bb := ((bb shl 6) or (bb shr (32 - 6))) + aa;
  dd := ((dd shl 10) or (dd shr (32 - 10)));

  aaa := aaa + (bbb xor (ccc or (not ddd))) + X[5] + $50A28BE6;
  aaa := ((aaa shl 8) or (aaa shr (32 - 8))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + (aaa xor (bbb or (not ccc))) + X[14] + $50A28BE6;
  eee := ((eee shl 9) or (eee shr (32 - 9))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + (eee xor (aaa or (not bbb))) + X[7] + $50A28BE6;
  ddd := ((ddd shl 9) or (ddd shr (32 - 9))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + (ddd xor (eee or (not aaa))) + X[0] + $50A28BE6;
  ccc := ((ccc shl 11) or (ccc shr (32 - 11))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + (ccc xor (ddd or (not eee))) + X[9] + $50A28BE6;
  bbb := ((bbb shl 13) or (bbb shr (32 - 13))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + (bbb xor (ccc or (not ddd))) + X[2] + $50A28BE6;
  aaa := ((aaa shl 15) or (aaa shr (32 - 15))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + (aaa xor (bbb or (not ccc))) + X[11] + $50A28BE6;
  eee := ((eee shl 15) or (eee shr (32 - 15))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + (eee xor (aaa or (not bbb))) + X[4] + $50A28BE6;
  ddd := ((ddd shl 5) or (ddd shr (32 - 5))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + (ddd xor (eee or (not aaa))) + X[13] + $50A28BE6;
  ccc := ((ccc shl 7) or (ccc shr (32 - 7))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + (ccc xor (ddd or (not eee))) + X[6] + $50A28BE6;
  bbb := ((bbb shl 7) or (bbb shr (32 - 7))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + (bbb xor (ccc or (not ddd))) + X[15] + $50A28BE6;
  aaa := ((aaa shl 8) or (aaa shr (32 - 8))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + (aaa xor (bbb or (not ccc))) + X[8] + $50A28BE6;
  eee := ((eee shl 11) or (eee shr (32 - 11))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + (eee xor (aaa or (not bbb))) + X[1] + $50A28BE6;
  ddd := ((ddd shl 14) or (ddd shr (32 - 14))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + (ddd xor (eee or (not aaa))) + X[10] + $50A28BE6;
  ccc := ((ccc shl 14) or (ccc shr (32 - 14))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + (ccc xor (ddd or (not eee))) + X[3] + $50A28BE6;
  bbb := ((bbb shl 12) or (bbb shr (32 - 12))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + (bbb xor (ccc or (not ddd))) + X[12] + $50A28BE6;
  aaa := ((aaa shl 6) or (aaa shr (32 - 6))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));

  eee := eee + ((aaa and ccc) or (bbb and (not ccc))) + X[6] + $5C4DD124;
  eee := ((eee shl 9) or (eee shr (32 - 9))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee and bbb) or (aaa and (not bbb))) + X[11] + $5C4DD124;
  ddd := ((ddd shl 13) or (ddd shr (32 - 13))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd and aaa) or (eee and (not aaa))) + X[3] + $5C4DD124;
  ccc := ((ccc shl 15) or (ccc shr (32 - 15))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc and eee) or (ddd and (not eee))) + X[7] + $5C4DD124;
  bbb := ((bbb shl 7) or (bbb shr (32 - 7))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[0] + $5C4DD124;
  aaa := ((aaa shl 12) or (aaa shr (32 - 12))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa and ccc) or (bbb and (not ccc))) + X[13] + $5C4DD124;
  eee := ((eee shl 8) or (eee shr (32 - 8))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee and bbb) or (aaa and (not bbb))) + X[5] + $5C4DD124;
  ddd := ((ddd shl 9) or (ddd shr (32 - 9))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd and aaa) or (eee and (not aaa))) + X[10] + $5C4DD124;
  ccc := ((ccc shl 11) or (ccc shr (32 - 11))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc and eee) or (ddd and (not eee))) + X[14] + $5C4DD124;
  bbb := ((bbb shl 7) or (bbb shr (32 - 7))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[15] + $5C4DD124;
  aaa := ((aaa shl 7) or (aaa shr (32 - 7))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa and ccc) or (bbb and (not ccc))) + X[8] + $5C4DD124;
  eee := ((eee shl 12) or (eee shr (32 - 12))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee and bbb) or (aaa and (not bbb))) + X[12] + $5C4DD124;
  ddd := ((ddd shl 7) or (ddd shr (32 - 7))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd and aaa) or (eee and (not aaa))) + X[4] + $5C4DD124;
  ccc := ((ccc shl 6) or (ccc shr (32 - 6))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc and eee) or (ddd and (not eee))) + X[9] + $5C4DD124;
  bbb := ((bbb shl 15) or (bbb shr (32 - 15))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[1] + $5C4DD124;
  aaa := ((aaa shl 13) or (aaa shr (32 - 13))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa and ccc) or (bbb and (not ccc))) + X[2] + $5C4DD124;
  eee := ((eee shl 11) or (eee shr (32 - 11))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));

  ddd := ddd + ((eee or (not aaa)) xor bbb) + X[15] + $6D703EF3;
  ddd := ((ddd shl 9) or (ddd shr (32 - 9))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd or (not eee)) xor aaa) + X[5] + $6D703EF3;
  ccc := ((ccc shl 7) or (ccc shr (32 - 7))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc or (not ddd)) xor eee) + X[1] + $6D703EF3;
  bbb := ((bbb shl 15) or (bbb shr (32 - 15))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb or (not ccc)) xor ddd) + X[3] + $6D703EF3;
  aaa := ((aaa shl 11) or (aaa shr (32 - 11))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa or (not bbb)) xor ccc) + X[7] + $6D703EF3;
  eee := ((eee shl 8) or (eee shr (32 - 8))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee or (not aaa)) xor bbb) + X[14] + $6D703EF3;
  ddd := ((ddd shl 6) or (ddd shr (32 - 6))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd or (not eee)) xor aaa) + X[6] + $6D703EF3;
  ccc := ((ccc shl 6) or (ccc shr (32 - 6))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc or (not ddd)) xor eee) + X[9] + $6D703EF3;
  bbb := ((bbb shl 14) or (bbb shr (32 - 14))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb or (not ccc)) xor ddd) + X[11] + $6D703EF3;
  aaa := ((aaa shl 12) or (aaa shr (32 - 12))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa or (not bbb)) xor ccc) + X[8] + $6D703EF3;
  eee := ((eee shl 13) or (eee shr (32 - 13))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee or (not aaa)) xor bbb) + X[12] + $6D703EF3;
  ddd := ((ddd shl 5) or (ddd shr (32 - 5))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd or (not eee)) xor aaa) + X[2] + $6D703EF3;
  ccc := ((ccc shl 14) or (ccc shr (32 - 14))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc or (not ddd)) xor eee) + X[10] + $6D703EF3;
  bbb := ((bbb shl 13) or (bbb shr (32 - 13))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb or (not ccc)) xor ddd) + X[0] + $6D703EF3;
  aaa := ((aaa shl 13) or (aaa shr (32 - 13))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa or (not bbb)) xor ccc) + X[4] + $6D703EF3;
  eee := ((eee shl 7) or (eee shr (32 - 7))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee or (not aaa)) xor bbb) + X[13] + $6D703EF3;
  ddd := ((ddd shl 5) or (ddd shr (32 - 5))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));

  ccc := ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[8] + $7A6D76E9;
  ccc := ((ccc shl 15) or (ccc shr (32 - 15))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[6] + $7A6D76E9;
  bbb := ((bbb shl 5) or (bbb shr (32 - 5))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[4] + $7A6D76E9;
  aaa := ((aaa shl 8) or (aaa shr (32 - 8))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[1] + $7A6D76E9;
  eee := ((eee shl 11) or (eee shr (32 - 11))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee and aaa) or ((not eee) and bbb)) + X[3] + $7A6D76E9;
  ddd := ((ddd shl 14) or (ddd shr (32 - 14))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[11] + $7A6D76E9;
  ccc := ((ccc shl 14) or (ccc shr (32 - 14))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[15] + $7A6D76E9;
  bbb := ((bbb shl 6) or (bbb shr (32 - 6))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[0] + $7A6D76E9;
  aaa := ((aaa shl 14) or (aaa shr (32 - 14))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[5] + $7A6D76E9;
  eee := ((eee shl 6) or (eee shr (32 - 6))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee and aaa) or ((not eee) and bbb)) + X[12] + $7A6D76E9;
  ddd := ((ddd shl 9) or (ddd shr (32 - 9))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[2] + $7A6D76E9;
  ccc := ((ccc shl 12) or (ccc shr (32 - 12))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[13] + $7A6D76E9;
  bbb := ((bbb shl 9) or (bbb shr (32 - 9))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[9] + $7A6D76E9;
  aaa := ((aaa shl 12) or (aaa shr (32 - 12))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[7] + $7A6D76E9;
  eee := ((eee shl 5) or (eee shr (32 - 5))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + ((eee and aaa) or ((not eee) and bbb)) + X[10] + $7A6D76E9;
  ddd := ((ddd shl 15) or (ddd shr (32 - 15))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[14] + $7A6D76E9;
  ccc := ((ccc shl 8) or (ccc shr (32 - 8))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));

  bbb := bbb + (ccc xor ddd xor eee) + X[12];
  bbb := ((bbb shl 8) or (bbb shr (32 - 8))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + (bbb xor ccc xor ddd) + X[15];
  aaa := ((aaa shl 5) or (aaa shr (32 - 5))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + (aaa xor bbb xor ccc) + X[10];
  eee := ((eee shl 12) or (eee shr (32 - 12))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + (eee xor aaa xor bbb) + X[4];
  ddd := ((ddd shl 9) or (ddd shr (32 - 9))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + (ddd xor eee xor aaa) + X[1];
  ccc := ((ccc shl 12) or (ccc shr (32 - 12))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + (ccc xor ddd xor eee) + X[5];
  bbb := ((bbb shl 5) or (bbb shr (32 - 5))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + (bbb xor ccc xor ddd) + X[8];
  aaa := ((aaa shl 14) or (aaa shr (32 - 14))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + (aaa xor bbb xor ccc) + X[7];
  eee := ((eee shl 6) or (eee shr (32 - 6))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + (eee xor aaa xor bbb) + X[6];
  ddd := ((ddd shl 8) or (ddd shr (32 - 8))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + (ddd xor eee xor aaa) + X[2];
  ccc := ((ccc shl 13) or (ccc shr (32 - 13))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + (ccc xor ddd xor eee) + X[13];
  bbb := ((bbb shl 6) or (bbb shr (32 - 6))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));
  aaa := aaa + (bbb xor ccc xor ddd) + X[14];
  aaa := ((aaa shl 5) or (aaa shr (32 - 5))) + eee;
  ccc := ((ccc shl 10) or (ccc shr (32 - 10)));
  eee := eee + (aaa xor bbb xor ccc) + X[0];
  eee := ((eee shl 15) or (eee shr (32 - 15))) + ddd;
  bbb := ((bbb shl 10) or (bbb shr (32 - 10)));
  ddd := ddd + (eee xor aaa xor bbb) + X[3];
  ddd := ((ddd shl 13) or (ddd shr (32 - 13))) + ccc;
  aaa := ((aaa shl 10) or (aaa shr (32 - 10)));
  ccc := ccc + (ddd xor eee xor aaa) + X[9];
  ccc := ((ccc shl 11) or (ccc shr (32 - 11))) + bbb;
  eee := ((eee shl 10) or (eee shr (32 - 10)));
  bbb := bbb + (ccc xor ddd xor eee) + X[11];
  bbb := ((bbb shl 11) or (bbb shr (32 - 11))) + aaa;
  ddd := ((ddd shl 10) or (ddd shr (32 - 10)));

  ddd := ddd + cc + CurrentHash[1];
  CurrentHash[1] := CurrentHash[2] + dd + eee;
  CurrentHash[2] := CurrentHash[3] + ee + aaa;
  CurrentHash[3] := CurrentHash[4] + aa + bbb;
  CurrentHash[4] := CurrentHash[0] + bb + ccc;
  CurrentHash[0] := ddd;
  FillChar(X, Sizeof(X), 0);
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TRMD160.InitHash;
begin
  Burn;
  CurrentHash[0] := $67452301;
  CurrentHash[1] := $EFCDAB89;
  CurrentHash[2] := $98BADCFE;
  CurrentHash[3] := $10325476;
  CurrentHash[4] := $C3D2E1F0;
  fInitialized := true;
end;

procedure TRMD160.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TRMD160.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi, Size shr 29);
  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TRMD160.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 56 then
    Compress;
  PDWord(@HashBuffer[56])^ := LenLo;
  PDWord(@HashBuffer[60])^ := LenHi;
  Compress;
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TSHA1.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewSHA1;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TSHA1.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

{$R-}{$Q-}

function SwapDWord(a: dword): dword; overload;
begin
  Result := ((a and $FF) shl 24) or ((a and $FF00) shl 8) or ((a and $FF0000) shr 8) or ((a and $FF000000) shr 24);
end;

function SwapDWORD(const a: Int64): Int64; overload;
begin
  Result := ((a and $FF) shl 56) or ((a and $FF00) shl 40) or ((a and $FF0000) shl 24) or ((a and $FF000000) shl 8) or
    ((a and $FF00000000) shr 8) or ((a and $FF0000000000) shr 24) or ((a and $FF000000000000) shr 40) or ((a and $FF00000000000000) shr 56);
end;

procedure TSHA1.InitHash;
begin
  Burn;
  CurrentHash[0] := $67452301;
  CurrentHash[1] := $EFCDAB89;
  CurrentHash[2] := $98BADCFE;
  CurrentHash[3] := $10325476;
  CurrentHash[4] := $C3D2E1F0;
  fInitialized := true;
end;

procedure TSHA1.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TSHA1.Compress;
var
  A, B, C, D, E     : DWord;
  W                 : array[0..79] of DWord;
  i                 : longword;
begin
  Index := 0;
  Move(HashBuffer, W, Sizeof(HashBuffer));
  for i := 0 to 15 do
    W[i] := SwapDWord(W[i]);
  for i := 16 to 79 do
    W[i] := ((W[i - 3] xor W[i - 8] xor W[i - 14] xor W[i - 16]) shl 1) or ((W[i - 3] xor W[i - 8] xor W[i - 14] xor W[i - 16]) shr 31);
  A := CurrentHash[0];
  B := CurrentHash[1];
  C := CurrentHash[2];
  D := CurrentHash[3];
  E := CurrentHash[4];

  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[0]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[1]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[2]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[3]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[4]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[5]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[6]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[7]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[8]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[9]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[10]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[11]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[12]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[13]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[14]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[15]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[16]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[17]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[18]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[19]);
  C := (C shl 30) or (C shr 2);

  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[20]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[21]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[22]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[23]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[24]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[25]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[26]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[27]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[28]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[29]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[30]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[31]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[32]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[33]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[34]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[35]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[36]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[37]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[38]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[39]);
  C := (C shl 30) or (C shr 2);

  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[40]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[41]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[42]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[43]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[44]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[45]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[46]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[47]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[48]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[49]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[50]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[51]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[52]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[53]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[54]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[55]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[56]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[57]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[58]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[59]);
  C := (C shl 30) or (C shr 2);

  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[60]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[61]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[62]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[63]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[64]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[65]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[66]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[67]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[68]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[69]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[70]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[71]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[72]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[73]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[74]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[75]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[76]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[77]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[78]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[79]);
  C := (C shl 30) or (C shr 2);

  CurrentHash[0] := CurrentHash[0] + A;
  CurrentHash[1] := CurrentHash[1] + B;
  CurrentHash[2] := CurrentHash[2] + C;
  CurrentHash[3] := CurrentHash[3] + D;
  CurrentHash[4] := CurrentHash[4] + E;
  FillChar(W, Sizeof(W), 0);
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TSHA1.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi, Size shr 29);
  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TSHA1.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 56 then
    Compress;
  PDWord(@HashBuffer[56])^ := SwapDWord(LenHi);
  PDWord(@HashBuffer[60])^ := SwapDWord(LenLo);
  Compress;
  CurrentHash[0] := SwapDWord(CurrentHash[0]);
  CurrentHash[1] := SwapDWord(CurrentHash[1]);
  CurrentHash[2] := SwapDWord(CurrentHash[2]);
  CurrentHash[3] := SwapDWord(CurrentHash[3]);
  CurrentHash[4] := SwapDWord(CurrentHash[4]);
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TSHA256.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewSHA256;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TSHA256.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

procedure TSHA256.Compress;
var
  a, b, c, d, e, f, g, h, t1, t2: DWord;
  W                 : array[0..63] of DWord;
  i                 : longword;
begin
  Index := 0;
  a := CurrentHash[0];
  b := CurrentHash[1];
  c := CurrentHash[2];
  d := CurrentHash[3];
  e := CurrentHash[4];
  f := CurrentHash[5];
  g := CurrentHash[6];
  h := CurrentHash[7];
  Move(HashBuffer, W, Sizeof(HashBuffer));
  for i := 0 to 15 do
    W[i] := SwapDWord(W[i]);
  for i := 16 to 63 do
    W[i] := (((W[i - 2] shr 17) or (W[i - 2] shl 15)) xor ((W[i - 2] shr 19) or (W[i - 2] shl 13)) xor
      (W[i - 2] shr 10)) + W[i - 7] + (((W[i - 15] shr 7) or (W[i - 15] shl 25)) xor
      ((W[i - 15] shr 18) or (W[i - 15] shl 14)) xor (W[i - 15] shr 3)) + W[i - 16];
  {
  Non-optimised version
    for i:= 0 to 63 do
    begin
      t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) +
        ((e and f) xor (not e and g)) + K[i] + W[i];
      t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) +
        ((a and b) xor (a and c) xor (b and c));
      h:= g; g:= f; f:= e; e:= d + t1; d:= c; c:= b; b:= a; a:= t1 + t2;
    end;
  }

  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $428A2F98 + W[0];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $71374491 + W[1];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $B5C0FBCF + W[2];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $E9B5DBA5 + W[3];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $3956C25B + W[4];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $59F111F1 + W[5];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $923F82A4 + W[6];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $AB1C5ED5 + W[7];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;
  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $D807AA98 + W[8];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $12835B01 + W[9];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $243185BE + W[10];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $550C7DC3 + W[11];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $72BE5D74 + W[12];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $80DEB1FE + W[13];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $9BDC06A7 + W[14];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $C19BF174 + W[15];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;
  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $E49B69C1 + W[16];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $EFBE4786 + W[17];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $0FC19DC6 + W[18];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $240CA1CC + W[19];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $2DE92C6F + W[20];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $4A7484AA + W[21];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $5CB0A9DC + W[22];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $76F988DA + W[23];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;
  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $983E5152 + W[24];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $A831C66D + W[25];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $B00327C8 + W[26];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $BF597FC7 + W[27];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $C6E00BF3 + W[28];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $D5A79147 + W[29];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $06CA6351 + W[30];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $14292967 + W[31];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;
  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $27B70A85 + W[32];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $2E1B2138 + W[33];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $4D2C6DFC + W[34];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $53380D13 + W[35];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $650A7354 + W[36];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $766A0ABB + W[37];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $81C2C92E + W[38];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $92722C85 + W[39];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;
  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $A2BFE8A1 + W[40];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $A81A664B + W[41];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $C24B8B70 + W[42];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $C76C51A3 + W[43];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $D192E819 + W[44];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $D6990624 + W[45];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $F40E3585 + W[46];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $106AA070 + W[47];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;
  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $19A4C116 + W[48];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $1E376C08 + W[49];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $2748774C + W[50];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $34B0BCB5 + W[51];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $391C0CB3 + W[52];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $4ED8AA4A + W[53];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $5B9CCA4F + W[54];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $682E6FF3 + W[55];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;
  t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $748F82EE + W[56];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := t1 + t2;
  d := d + t1;
  t1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $78A5636F + W[57];
  t2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := t1 + t2;
  c := c + t1;
  t1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $84C87814 + W[58];
  t2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := t1 + t2;
  b := b + t1;
  t1 := e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $8CC70208 + W[59];
  t2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  e := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $90BEFFFA + W[60];
  t2 := (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g));
  d := t1 + t2;
  h := h + t1;
  t1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $A4506CEB + W[61];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f));
  c := t1 + t2;
  g := g + t1;
  t1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $BEF9A3F7 + W[62];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e));
  b := t1 + t2;
  f := f + t1;
  t1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $C67178F2 + W[63];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  e := e + t1;

  CurrentHash[0] := CurrentHash[0] + a;
  CurrentHash[1] := CurrentHash[1] + b;
  CurrentHash[2] := CurrentHash[2] + c;
  CurrentHash[3] := CurrentHash[3] + d;
  CurrentHash[4] := CurrentHash[4] + e;
  CurrentHash[5] := CurrentHash[5] + f;
  CurrentHash[6] := CurrentHash[6] + g;
  CurrentHash[7] := CurrentHash[7] + h;
  FillChar(W, Sizeof(W), 0);
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TSHA256.InitHash;
begin
  Burn;
  CurrentHash[0] := $6A09E667;
  CurrentHash[1] := $BB67AE85;
  CurrentHash[2] := $3C6EF372;
  CurrentHash[3] := $A54FF53A;
  CurrentHash[4] := $510E527F;
  CurrentHash[5] := $9B05688C;
  CurrentHash[6] := $1F83D9AB;
  CurrentHash[7] := $5BE0CD19;
  fInitialized := true;
end;

procedure TSHA256.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TSHA256.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  ///  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi, Size shr 29);
  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TSHA256.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 56 then
    Compress;
  PDWord(@HashBuffer[56])^ := SwapDWord(LenHi);
  PDWord(@HashBuffer[60])^ := SwapDWord(LenLo);
  Compress;
  CurrentHash[0] := SwapDWord(CurrentHash[0]);
  CurrentHash[1] := SwapDWord(CurrentHash[1]);
  CurrentHash[2] := SwapDWord(CurrentHash[2]);
  CurrentHash[3] := SwapDWord(CurrentHash[3]);
  CurrentHash[4] := SwapDWord(CurrentHash[4]);
  CurrentHash[5] := SwapDWord(CurrentHash[5]);
  CurrentHash[6] := SwapDWord(CurrentHash[6]);
  CurrentHash[7] := SwapDWord(CurrentHash[7]);
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TSHA384.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;
  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewSHA384;
begin
  New(Result, Create);

  // code
end;
////////////////////////////////////////////////////////////////////////////////

{$R-}{$Q-}

procedure TSHA384.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

procedure TSHA384.Compress;
var
  a, b, c, d, e, f, g, h, t1, t2: int64;
  W                 : array[0..79] of int64;
  i                 : longword;
begin
  Index := 0;
  a := CurrentHash[0];
  b := CurrentHash[1];
  c := CurrentHash[2];
  d := CurrentHash[3];
  e := CurrentHash[4];
  f := CurrentHash[5];
  g := CurrentHash[6];
  h := CurrentHash[7];
  Move(HashBuffer, W, Sizeof(HashBuffer));
  for i := 0 to 15 do
    W[i] := SwapDWord(W[i]);
  for i := 16 to 79 do
    W[i] := (((W[i - 2] shr 19) or (W[i - 2] shl 45)) xor ((W[i - 2] shr 61) or (W[i - 2] shl 3)) xor
      (W[i - 2] shr 6)) + W[i - 7] + (((W[i - 15] shr 1) or (W[i - 15] shl 63)) xor ((W[i - 15] shr 8) or
      (W[i - 15] shl 56)) xor (W[i - 15] shr 7)) + W[i - 16];

  {
  Non-optimised version
    for i:= 0 to 79 do
    begin
      t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) +
        ((e and f) xor (not e and g)) + K[i] + W[i];
      t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) +
        ((a and b) xor (a and c) xor (b and c));
      h:= g; g:= f; f:= e; e:= d + t1; d:= c; c:= b; b:= a; a:= t1 + t2;
    end;
  }

  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $428A2F98D728AE22 + W[0];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $7137449123EF65CD + W[1];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $B5C0FBCFEC4D3B2F + W[2];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $E9B5DBA58189DBBC + W[3];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $3956C25BF348B538 + W[4];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $59F111F1B605D019 + W[5];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $923F82A4AF194F9B + W[6];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $AB1C5ED5DA6D8118 + W[7];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $D807AA98A3030242 + W[8];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $12835B0145706FBE + W[9];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $243185BE4EE4B28C + W[10];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $550C7DC3D5FFB4E2 + W[11];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $72BE5D74F27B896F + W[12];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $80DEB1FE3B1696B1 + W[13];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $9BDC06A725C71235 + W[14];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $C19BF174CF692694 + W[15];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $E49B69C19EF14AD2 + W[16];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $EFBE4786384F25E3 + W[17];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $0FC19DC68B8CD5B5 + W[18];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $240CA1CC77AC9C65 + W[19];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $2DE92C6F592B0275 + W[20];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4A7484AA6EA6E483 + W[21];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5CB0A9DCBD41FBD4 + W[22];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $76F988DA831153B5 + W[23];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $983E5152EE66DFAB + W[24];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $A831C66D2DB43210 + W[25];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $B00327C898FB213F + W[26];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $BF597FC7BEEF0EE4 + W[27];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $C6E00BF33DA88FC2 + W[28];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $D5A79147930AA725 + W[29];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $06CA6351E003826F + W[30];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $142929670A0E6E70 + W[31];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $27B70A8546D22FFC + W[32];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $2E1B21385C26C926 + W[33];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $4D2C6DFC5AC42AED + W[34];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $53380D139D95B3DF + W[35];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $650A73548BAF63DE + W[36];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $766A0ABB3C77B2A8 + W[37];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $81C2C92E47EDAEE6 + W[38];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $92722C851482353B + W[39];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $A2BFE8A14CF10364 + W[40];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $A81A664BBC423001 + W[41];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $C24B8B70D0F89791 + W[42];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $C76C51A30654BE30 + W[43];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $D192E819D6EF5218 + W[44];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $D69906245565A910 + W[45];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $F40E35855771202A + W[46];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $106AA07032BBD1B8 + W[47];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $19A4C116B8D2D0C8 + W[48];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $1E376C085141AB53 + W[49];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $2748774CDF8EEB99 + W[50];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $34B0BCB5E19B48A8 + W[51];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $391C0CB3C5C95A63 + W[52];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4ED8AA4AE3418ACB + W[53];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5B9CCA4F7763E373 + W[54];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $682E6FF3D6B2B8A3 + W[55];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $748F82EE5DEFB2FC + W[56];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $78A5636F43172F60 + W[57];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $84C87814A1F0AB72 + W[58];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $8CC702081A6439EC + W[59];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $90BEFFFA23631E28 + W[60];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $A4506CEBDE82BDE9 + W[61];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $BEF9A3F7B2C67915 + W[62];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $C67178F2E372532B + W[63];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $CA273ECEEA26619C + W[64];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $D186B8C721C0C207 + W[65];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $EADA7DD6CDE0EB1E + W[66];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $F57D4F7FEE6ED178 + W[67];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $06F067AA72176FBA + W[68];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $0A637DC5A2C898A6 + W[69];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $113F9804BEF90DAE + W[70];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $1B710B35131C471B + W[71];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $28DB77F523047D84 + W[72];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $32CAAB7B40C72493 + W[73];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $3C9EBE0A15C9BEBC + W[74];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $431D67C49C100D4C + W[75];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $4CC5D4BECB3E42B6 + W[76];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $597F299CFC657E2A + W[77];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5FCB6FAB3AD6FAEC + W[78];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $6C44198C4A475817 + W[79];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;

  CurrentHash[0] := CurrentHash[0] + a;
  CurrentHash[1] := CurrentHash[1] + b;
  CurrentHash[2] := CurrentHash[2] + c;
  CurrentHash[3] := CurrentHash[3] + d;
  CurrentHash[4] := CurrentHash[4] + e;
  CurrentHash[5] := CurrentHash[5] + f;
  CurrentHash[6] := CurrentHash[6] + g;
  CurrentHash[7] := CurrentHash[7] + h;
  FillChar(W, Sizeof(W), 0);
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TSHA384.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TSHA384.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
   //   raise EDCP_hash.Create('Hash not initialized');

  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then
    Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TSHA384.InitHash;
begin
  Burn;
{$WARNINGS OFF}
  CurrentHash[0] := $CBBB9D5DC1059ED8;
  CurrentHash[1] := $629A292A367CD507;
  CurrentHash[2] := $9159015A3070DD17;
  CurrentHash[3] := $152FECD8F70E5939;
  CurrentHash[4] := $67332667FFC00B31;
  CurrentHash[5] := $8EB44A8768581511;
  CurrentHash[6] := $DB0C2E0D64F98FA7;
  CurrentHash[7] := $47B5481DBEFA4FA4;
{$WARNINGS ON}
  fInitialized := true;
end;

procedure TSHA384.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 112 then
    Compress;
  Pint64(@HashBuffer[112])^ := SwapDWord(LenHi);
  Pint64(@HashBuffer[120])^ := SwapDWord(LenLo);
  Compress;
  CurrentHash[0] := SwapDWord(CurrentHash[0]);
  CurrentHash[1] := SwapDWord(CurrentHash[1]);
  CurrentHash[2] := SwapDWord(CurrentHash[2]);
  CurrentHash[3] := SwapDWord(CurrentHash[3]);
  CurrentHash[4] := SwapDWord(CurrentHash[4]);
  CurrentHash[5] := SwapDWord(CurrentHash[5]);
  Move(CurrentHash, Digest, 384 div 8);
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TSHA512.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then Burn;
  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewSHA512;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TSHA512.UpdateStr(const s: string);
begin
  Update(s[1], Length(s));
end;

procedure TSHA512.Compress;
var
  a, b, c, d, e, f, g, h, t1, t2: Int64;
  W                 : array[0..79] of Int64;
  i                 : DWORD;
begin
  Index := 0;
  a := CurrentHash[0];
  b := CurrentHash[1];
  c := CurrentHash[2];
  d := CurrentHash[3];
  e := CurrentHash[4];
  f := CurrentHash[5];
  g := CurrentHash[6];
  h := CurrentHash[7];
  Move(HashBuffer, W, Sizeof(HashBuffer));
  for i := 0 to 15 do W[i] := SwapDWORD(W[i]);
  for i := 16 to 79 do
  begin
    W[i] := (((W[i - 2] shr 19) or (W[i - 2] shl 45)) xor ((W[i - 2] shr 61) or (W[i - 2] shl 3)) xor
      (W[i - 2] shr 6)) + W[i - 7] + (((W[i - 15] shr 1) or (W[i - 15] shl 63)) xor ((W[i - 15] shr 8) or (W[i - 15] shl 56)) xor (W[i - 15] shr 7)) + W[i - 16];
  end;

  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $428A2F98D728AE22 + W[0];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $7137449123EF65CD + W[1];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $B5C0FBCFEC4D3B2F + W[2];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $E9B5DBA58189DBBC + W[3];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $3956C25BF348B538 + W[4];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $59F111F1B605D019 + W[5];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $923F82A4AF194F9B + W[6];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $AB1C5ED5DA6D8118 + W[7];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $D807AA98A3030242 + W[8];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $12835B0145706FBE + W[9];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $243185BE4EE4B28C + W[10];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $550C7DC3D5FFB4E2 + W[11];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $72BE5D74F27B896F + W[12];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $80DEB1FE3B1696B1 + W[13];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $9BDC06A725C71235 + W[14];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $C19BF174CF692694 + W[15];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $E49B69C19EF14AD2 + W[16];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $EFBE4786384F25E3 + W[17];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $0FC19DC68B8CD5B5 + W[18];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $240CA1CC77AC9C65 + W[19];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $2DE92C6F592B0275 + W[20];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4A7484AA6EA6E483 + W[21];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5CB0A9DCBD41FBD4 + W[22];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $76F988DA831153B5 + W[23];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $983E5152EE66DFAB + W[24];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $A831C66D2DB43210 + W[25];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $B00327C898FB213F + W[26];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $BF597FC7BEEF0EE4 + W[27];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $C6E00BF33DA88FC2 + W[28];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $D5A79147930AA725 + W[29];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $06CA6351E003826F + W[30];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $142929670A0E6E70 + W[31];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $27B70A8546D22FFC + W[32];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $2E1B21385C26C926 + W[33];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $4D2C6DFC5AC42AED + W[34];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $53380D139D95B3DF + W[35];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $650A73548BAF63DE + W[36];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $766A0ABB3C77B2A8 + W[37];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $81C2C92E47EDAEE6 + W[38];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $92722C851482353B + W[39];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $A2BFE8A14CF10364 + W[40];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $A81A664BBC423001 + W[41];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $C24B8B70D0F89791 + W[42];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $C76C51A30654BE30 + W[43];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $D192E819D6EF5218 + W[44];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $D69906245565A910 + W[45];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $F40E35855771202A + W[46];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $106AA07032BBD1B8 + W[47];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $19A4C116B8D2D0C8 + W[48];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $1E376C085141AB53 + W[49];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $2748774CDF8EEB99 + W[50];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $34B0BCB5E19B48A8 + W[51];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $391C0CB3C5C95A63 + W[52];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4ED8AA4AE3418ACB + W[53];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5B9CCA4F7763E373 + W[54];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $682E6FF3D6B2B8A3 + W[55];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $748F82EE5DEFB2FC + W[56];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $78A5636F43172F60 + W[57];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $84C87814A1F0AB72 + W[58];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $8CC702081A6439EC + W[59];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $90BEFFFA23631E28 + W[60];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $A4506CEBDE82BDE9 + W[61];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $BEF9A3F7B2C67915 + W[62];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $C67178F2E372532B + W[63];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $CA273ECEEA26619C + W[64];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $D186B8C721C0C207 + W[65];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $EADA7DD6CDE0EB1E + W[66];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $F57D4F7FEE6ED178 + W[67];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $06F067AA72176FBA + W[68];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $0A637DC5A2C898A6 + W[69];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $113F9804BEF90DAE + W[70];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $1B710B35131C471B + W[71];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;
  t1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $28DB77F523047D84 + W[72];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  h := t1 + t2;
  t1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $32CAAB7B40C72493 + W[73];
  t2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + t1;
  g := t1 + t2;
  t1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $3C9EBE0A15C9BEBC + W[74];
  t2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + t1;
  f := t1 + t2;
  t1 := e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $431D67C49C100D4C + W[75];
  t2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + t1;
  e := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $4CC5D4BECB3E42B6 + W[76];
  t2 := (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g));
  h := h + t1;
  d := t1 + t2;
  t1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $597F299CFC657E2A + W[77];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f));
  g := g + t1;
  c := t1 + t2;
  t1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5FCB6FAB3AD6FAEC + W[78];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e));
  f := f + t1;
  b := t1 + t2;
  t1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $6C44198C4A475817 + W[79];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  e := e + t1;
  a := t1 + t2;

  Inc(CurrentHash[0], a);
  Inc(CurrentHash[1], b);
  Inc(CurrentHash[2], c);
  Inc(CurrentHash[3], d);
  Inc(CurrentHash[4], e);
  Inc(CurrentHash[5], f);
  Inc(CurrentHash[6], g);
  Inc(CurrentHash[7], h);
  FillChar(W, Sizeof(W), 0);
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TSHA512.Burn;
begin
  LenHi := 0;
  LenLo := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := False;
end;

procedure TSHA512.Update(const Buffer; Size: DWORD);
var
  PBuf              : PByte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenLo, Size * 8);
  if LenLo < (Size * 8) then Inc(LenHi);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWORD(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TSHA512.InitHash;
begin
  Burn;
{$WARNINGS OFF}
  CurrentHash[0] := $6A09E667F3BCC908;
  CurrentHash[1] := $BB67AE8584CAA73B;
  CurrentHash[2] := $3C6EF372FE94F82B;
  CurrentHash[3] := $A54FF53A5F1D36F1;
  CurrentHash[4] := $510E527FADE682D1;
  CurrentHash[5] := $9B05688C2B3E6C1F;
  CurrentHash[6] := $1F83D9ABFB41BD6B;
  CurrentHash[7] := $5BE0CD19137E2179;
{$WARNINGS ON}
  fInitialized := True;
end;

procedure TSHA512.Final(var Digest);
var
  i                 : Integer;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $80;
  if Index >= 112 then Compress;
  PInt64(@HashBuffer[112])^ := SwapDWORD(LenHi);
  PInt64(@HashBuffer[120])^ := SwapDWORD(LenLo);
  Compress;
  for i := 0 to 7 do CurrentHash[i] := SwapDWORD(CurrentHash[i]);
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

{--------------------}
{ Destructor Œ¡⁄≈ “¿ }
{--------------------}

destructor TTIGER.Destroy;
begin
  // All Strings := '';
  // Free_And_Nil(All PObj);
  if fInitialized then
    Burn;

  inherited;
end;
////////////////////////////////////////////////////////////////////////////////

{-----------------------------}
{  ŒÕ—“–” “Œ– ƒÀﬂ KOL Œ¡⁄≈ “¿ }
{-----------------------------}

function NewTIGER;
begin
  New(Result, Create);
  //burn;
  // code
end;
////////////////////////////////////////////////////////////////////////////////

procedure TTIGER.UpdateStr(const Str: string);
begin
  Update(Str[1], Length(Str));
end;

procedure TTIGER.Compress;
var
  a, b, c, aa, bb, cc: int64;
  x                 : array[0..7] of int64;
begin
  a := CurrentHash[0];
  aa := a;
  b := CurrentHash[1];
  bb := b;
  c := CurrentHash[2];
  cc := c;

  Move(HashBuffer, x, Sizeof(x));

  c := c xor x[0];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 5;
  a := a xor x[1];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 5;
  b := b xor x[2];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 5;
  c := c xor x[3];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 5;
  a := a xor x[4];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 5;
  b := b xor x[5];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 5;
  c := c xor x[6];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 5;
  a := a xor x[7];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 5;
  x[0] := x[0] - (x[7] xor $A5A5A5A5A5A5A5A5);
  x[1] := x[1] xor x[0];
  x[2] := x[2] + x[1];
  x[3] := x[3] - (x[2] xor ((not x[1]) shl 19));
  x[4] := x[4] xor x[3];
  x[5] := x[5] + x[4];
  x[6] := x[6] - (x[5] xor ((not x[4]) shr 23));
  x[7] := x[7] xor x[6];
  x[0] := x[0] + x[7];
  x[1] := x[1] - (x[0] xor ((not x[7]) shl 19));
  x[2] := x[2] xor x[1];
  x[3] := x[3] + x[2];
  x[4] := x[4] - (x[3] xor ((not x[2]) shr 23));
  x[5] := x[5] xor x[4];
  x[6] := x[6] + x[5];
  x[7] := x[7] - (x[6] xor $0123456789ABCDEF);
  b := b xor x[0];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 7;
  c := c xor x[1];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 7;
  a := a xor x[2];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 7;
  b := b xor x[3];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 7;
  c := c xor x[4];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 7;
  a := a xor x[5];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 7;
  b := b xor x[6];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 7;
  c := c xor x[7];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 7;
  x[0] := x[0] - (x[7] xor $A5A5A5A5A5A5A5A5);
  x[1] := x[1] xor x[0];
  x[2] := x[2] + x[1];
  x[3] := x[3] - (x[2] xor ((not x[1]) shl 19));
  x[4] := x[4] xor x[3];
  x[5] := x[5] + x[4];
  x[6] := x[6] - (x[5] xor ((not x[4]) shr 23));
  x[7] := x[7] xor x[6];
  x[0] := x[0] + x[7];
  x[1] := x[1] - (x[0] xor ((not x[7]) shl 19));
  x[2] := x[2] xor x[1];
  x[3] := x[3] + x[2];
  x[4] := x[4] - (x[3] xor ((not x[2]) shr 23));
  x[5] := x[5] xor x[4];
  x[6] := x[6] + x[5];
  x[7] := x[7] - (x[6] xor $0123456789ABCDEF);
  a := a xor x[0];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 9;
  b := b xor x[1];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 9;
  c := c xor x[2];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 9;
  a := a xor x[3];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 9;
  b := b xor x[4];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 9;
  c := c xor x[5];
  a := a - (tiger1[c and $FF] xor tiger2[(c shr 16) and $FF] xor tiger3[(c shr 32) and $FF] xor tiger4[(c shr 48) and $FF]);
  b := b + (tiger4[(c shr 8) and $FF] xor tiger3[(c shr 24) and $FF] xor tiger2[(c shr 40) and $FF] xor tiger1[(c shr 56) and $FF]);
  b := b * 9;
  a := a xor x[6];
  b := b - (tiger1[a and $FF] xor tiger2[(a shr 16) and $FF] xor tiger3[(a shr 32) and $FF] xor tiger4[(a shr 48) and $FF]);
  c := c + (tiger4[(a shr 8) and $FF] xor tiger3[(a shr 24) and $FF] xor tiger2[(a shr 40) and $FF] xor tiger1[(a shr 56) and $FF]);
  c := c * 9;
  b := b xor x[7];
  c := c - (tiger1[b and $FF] xor tiger2[(b shr 16) and $FF] xor tiger3[(b shr 32) and $FF] xor tiger4[(b shr 48) and $FF]);
  a := a + (tiger4[(b shr 8) and $FF] xor tiger3[(b shr 24) and $FF] xor tiger2[(b shr 40) and $FF] xor tiger1[(b shr 56) and $FF]);
  a := a * 9;

  CurrentHash[0] := a xor aa;
  CurrentHash[1] := b - bb;
  CurrentHash[2] := c + cc;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

procedure TTIGER.InitHash;
begin
  Burn;
  fInitialized := true;
{$WARNINGS OFF}
  CurrentHash[0] := $0123456789ABCDEF;
  CurrentHash[1] := $FEDCBA9876543210;
  CurrentHash[2] := $F096A5B4C3B2E187;
{$WARNINGS ON}
end;

procedure TTIGER.Burn;
begin
  Len := 0;
  Index := 0;
  FillChar(HashBuffer, Sizeof(HashBuffer), 0);
  FillChar(CurrentHash, Sizeof(CurrentHash), 0);
  fInitialized := false;
end;

procedure TTIGER.Update(const Buffer; Size: longword);
var
  PBuf              : ^byte;
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');

  Inc(Len, Size * 8);

  PBuf := @Buffer;
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
    begin
      Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
      Dec(Size, Sizeof(HashBuffer) - Index);
      Inc(PBuf, Sizeof(HashBuffer) - Index);
      Compress;
    end
    else
    begin
      Move(PBuf^, HashBuffer[Index], Size);
      Inc(Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TTIGER.Final(var Digest);
begin
  //  if not fInitialized then
  //    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index] := $01;
  if Index >= 56 then
    Compress;
  Pint64(@HashBuffer[56])^ := Len;
  Compress;
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

end.

