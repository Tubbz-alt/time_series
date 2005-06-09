# Microsoft Developer Studio Generated NMAKE File, Based on tstest1.dsp
!IF "$(CFG)" == ""
CFG=tstest1 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to tstest1 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "tstest1 - Win32 Release" && "$(CFG)" !=\
 "tstest1 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "tstest1.mak" CFG="tstest1 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "tstest1 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "tstest1 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "tstest1 - Win32 Release"

OUTDIR=.
INTDIR=.\Release
# Begin Custom Macros
OutDir=.
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\tstest1.exe"

!ELSE 

ALL : "library - Win32 Release" "$(OUTDIR)\tstest1.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"library - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\tstest1.obj"
	-@erase "$(OUTDIR)\tstest1.exe"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /warn:nofileopt\
 /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\tstest1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\tstest1.pdb" /machine:I386 /out:"$(OUTDIR)\tstest1.exe" 
LINK32_OBJS= \
	"$(INTDIR)\tstest1.obj" \
	"$(OUTDIR)\libts.lib"

"$(OUTDIR)\tstest1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "tstest1 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\tstest1.exe"

!ELSE 

ALL : "library - Win32 Debug" "$(OUTDIR)\tstest1.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"library - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\tstest1.obj"
	-@erase "$(OUTDIR)\tstest1.exe"
	-@erase "$(OUTDIR)\tstest1.ilk"
	-@erase "$(OUTDIR)\tstest1.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /debug:full /optimize:0\
 /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF50.PDB" 
F90_OBJS=.\Debug/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\tstest1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\tstest1.pdb" /debug /machine:I386 /out:"$(OUTDIR)\tstest1.exe"\
 /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\tstest1.obj" \
	".\libts.lib"

"$(OUTDIR)\tstest1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(CFG)" == "tstest1 - Win32 Release" || "$(CFG)" ==\
 "tstest1 - Win32 Debug"

!IF  "$(CFG)" == "tstest1 - Win32 Release"

"library - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\library.mak" CFG="library - Win32 Release" 
   cd "."

"library - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) CLEAN /F ".\library.mak" CFG="library - Win32 Release"\
 RECURSE=1 
   cd "."

!ELSEIF  "$(CFG)" == "tstest1 - Win32 Debug"

"library - Win32 Debug" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\library.mak" CFG="library - Win32 Debug" 
   cd "."

"library - Win32 DebugCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) CLEAN /F ".\library.mak" CFG="library - Win32 Debug"\
 RECURSE=1 
   cd "."

!ENDIF 

SOURCE=.\tstest1.f90

!IF  "$(CFG)" == "tstest1 - Win32 Release"

DEP_F90_TSTES=\
	".\Release\time_series.mod"\
	

"$(INTDIR)\tstest1.obj" : $(SOURCE) $(DEP_F90_TSTES) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tstest1 - Win32 Debug"

DEP_F90_TSTES=\
	".\Debug\time_series.mod"\
	

"$(INTDIR)\tstest1.obj" : $(SOURCE) $(DEP_F90_TSTES) "$(INTDIR)"


!ENDIF 


!ENDIF 

