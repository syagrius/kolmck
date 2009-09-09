# ******************************************************************
# *                                                                *
# *     DELPHI7 KOL RTL Replacement                                *
# *     MAKE script                                                *
# *                                                                *
# *     (C) 2005 by ECM                                            *
# *                                                                *
# ******************************************************************

# To build the runtime library without debug information (the default),
# simply run MAKE.EXE from the directory containing this MAKEFILE. To
# build a debug version of the runtime library, specify a -DDEBUG command
# line parameter when running MAKE.EXE.

DCC = dcc32 -q

BIN = bin
LIB = lib

!if $d(DEBUG)
RTLDEBUG = -$$D+
!else
RTLDEBUG = -$$D-
!endif

default: \
system.dcu   \
SysConst.dcu \
SysSfIni.dcu  \
SysWStr.dcu   


system.dcu: system.pas sysinit.pas getmem.inc
 $(DCC) system -m -y -z $(RTLDEBUG) 

SysConst.dcu: SysConst.pas system.dcu
 $(DCC) SysConst -z $(RTLDEBUG)

SysSfIni.dcu: SysSfIni.pas
 $(DCC) SysSfIni -z $(RTLDEBUG)

SysWStr.dcu: SysWStr.pas
 $(DCC) SysWStr -z $(RTLDEBUG) 