{$IFDEF VER130} // Delphi 5
  {$DEFINE _D5}
{$ENDIF}

{$IFDEF VER140} // Delphi 6
  {$DEFINE _D6orHigher}
  {$DEFINE _D6}
{$ENDIF}

{$IFDEF VER150} // Delphi 7
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D7}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER160} // Delphi 8
  Delphi version 8 not supported! (delphi 8 is .net only)
{$ENDIF}

{$IFDEF VER170} // Delphi 2005
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2005}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER180} // Delphi 2006 or Turbo
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2006}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER185} // Delphi 2007
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2007}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER200} // Delphi 2009 (first unicode version)
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2009}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER210} // Delphi 2010
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _D2010}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER220} // Delphi XE
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER230} // Delphi XE2
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE2}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER240} // Delphi XE3
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE3}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER250} // Delphi XE4
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE4}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER260} // Delphi XE5
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE5}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER270} // Delphi XE6
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE6}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER280} // Delphi XE7
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE7orHigher}
  {$DEFINE _DXE7}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER290} // Delphi XE8
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE7orHigher}
  {$DEFINE _DXE8orHigher}
  {$DEFINE _DXE8}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER300} // Delphi 10 Seattle
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE7orHigher}
  {$DEFINE _DXE8orHigher}
  {$DEFINE _D10orHigher}
  {$DEFINE _D10}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER310} // Delphi 10.1 Berlin
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE7orHigher}
  {$DEFINE _DXE8orHigher}
  {$DEFINE _D10orHigher}
  {$DEFINE _D10_1orHigher}
  {$DEFINE _D10_1}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER320} // Delphi 10.2 Tokyo
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE7orHigher}
  {$DEFINE _DXE8orHigher}
  {$DEFINE _D10orHigher}
  {$DEFINE _D10_1orHigher}
  {$DEFINE _D10_2orHigher}
  {$DEFINE _D10_2}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER330} // Delphi 10.3 Rio
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE7orHigher}
  {$DEFINE _DXE8orHigher}
  {$DEFINE _D10orHigher}
  {$DEFINE _D10_1orHigher}
  {$DEFINE _D10_2orHigher}
  {$DEFINE _D10_3orHigher}
  {$DEFINE _D10_3}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF VER340} // Delphi 10.4 Sydney
  {$DEFINE _D6orHigher}
  {$DEFINE _D7orHigher}
  {$DEFINE _D2005orHigher}
  {$DEFINE _D2006orHigher}
  {$DEFINE _D2007orHigher}
  {$DEFINE _D2009orHigher}
  {$DEFINE _D2010orHigher}
  {$DEFINE _DXEorHigher}
  {$DEFINE _DXE2orHigher}
  {$DEFINE _DXE3orHigher}
  {$DEFINE _DXE4orHigher}
  {$DEFINE _DXE5orHigher}
  {$DEFINE _DXE6orHigher}
  {$DEFINE _DXE7orHigher}
  {$DEFINE _DXE8orHigher}
  {$DEFINE _D10orHigher}
  {$DEFINE _D10_1orHigher}
  {$DEFINE _D10_2orHigher}
  {$DEFINE _D10_3orHigher}
  {$DEFINE _D10_4orHigher}
  {$DEFINE _D10_4}

  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}

  {$DEFINE TMSG_WINDOWS}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$ASMMODE INTEL}
  {$DEFINE USE_OLD_FLAGS} //size of set type in fpc is 4 bytes
  {------------------------------------
  by Thaddy de Koning:
  FPC version 2.1.1 is very compatible with Delphi and kol now.
  You can simply use the $(DELPHI)\source\rtl\win\*.pas files from Delphi 4/5 instead of the prepared files that were needed for
  FPC1.X
  That is all to have full compatibility.
  ------------------------------------}
  {$DEFINE PAS_VERSION}
  {$IFDEF VER2}
    {$DEFINE _D6orHigher}
    {$DEFINE _D7}
    {$DEFINE _D7orHigher}
  {$ENDIF}
  {$IFDEF VER3}
    // I should clean this up later.
    {$DEFINE VER2}
    {$DEFINE _D6orHigher}
    {$DEFINE _D7}
    {$DEFINE _D7orHigher}
    {$DEFINE _D2005orHigher}
    {$DEFINE _D2006orHigher}
    {$DEFINE _D2007orHigher}
  {$ENDIF}
{$ENDIF FPC}

{$IFNDEF _NOT_KOLCtrlWrapper_}
 {$DEFINE _KOLCtrlWrapper_}
{$ENDIF}

//// from delphidef.inc ////

//{$DEFINE USE_CUSTOMEXTENSIONS}
// Uncomment this option or add it to your project conditional defines,
// if You wish to extend existing TControl object from
// the inner of those. When this option is turned on, include directive at the
// tail of TControl declaration is enabled, causing a compiler to include your
// portion of source directly into the TControl body. See comments near this
// directive there. (Search the word: USE_CUSTOMEXTENSIONS).
// Please note, that this option is not fully supported now.

{$IFNDEF USE_OLD_FLAGS}
  {$DEFINE USE_FLAGS}
{$ELSE}
  {$UNDEF  USE_FLAGS}
{$ENDIF}

{$IFNDEF EVENTS_STATIC}
  {$DEFINE EVENTS_DYNAMIC}
{$ENDIF}

{$DEFINE PACK_COMMANDACTIONS}
{$IFDEF  NOT_PACK_COMMANDACTIONS}
  {$UNDEF PACK_COMMANDACTIONS}
{$ENDIF}

// AUTO UNICODE FOR DELPHI VERSION >= 2009
{$IFDEF _D2009orHigher}
  {$DEFINE UNICODE_CTRLS}
  {$DEFINE UStr_} // use functions @UStrXXXX instead of @WStrXXXX
{$ENDIF}

// PAS ONLY FOR X64
{$IFDEF WIN64}
  {$DEFINE PAS_VERSION}
  {$ALIGN ON}
  {$Z1} // MinEnumSize
{$ELSE}
  {$A-} // align off, otherwise code is not good
  {$Q-} // no overflow check: this option makes code wrong
  {$R-} // no range checking: this option makes code wrong
  {$Z-}
{$ENDIF}

{$T-} // not typed @-operator

// PAS ONLY FOR PACKAGE COMPILE
{$IFDEF INPACKAGE} 
  {$WARNINGS OFF}
  {$DEFINE PAS_VERSION}
{$ENDIF}

// USE ASM OR PASCAL CODE
{$IFDEF PAS_VERSION}
  {$UNDEF ASM_VERSION}
  {$UNDEF ASM_UNICODE}
  {$UNDEF ASM_TLIST}
{$ELSE}
  {$DEFINE ASM_VERSION}
  // asm without unicode
  {$IFNDEF UNICODE_CTRLS}
    {$DEFINE ASM_UNICODE}
  {$ELSE}
    {$UNDEF ASM_UNICODE}
  {$ENDIF}
  // asm TList
  {$DEFINE ASM_TLIST}
  {$IFDEF TLIST_FAST}
    {$UNDEF ASM_TLIST}
  {$ENDIF}
{$ENDIF}
