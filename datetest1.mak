# Microsoft Developer Studio Generated NMAKE File, Based on datetest1.dsp
!IF "$(CFG)" == ""
CFG=datetest1 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to datetest1 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "datetest1 - Win32 Release" && "$(CFG)" !=\
 "datetest1 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "datetest1.mak" CFG="datetest1 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "datetest1 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "datetest1 - Win32 Debug" (based on "Win32 (x86) Console Application")
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

!IF  "$(CFG)" == "datetest1 - Win32 Release"

OUTDIR=.
INTDIR=.\Release
# Begin Custom Macros
OutDir=.
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\datetest1.exe"

!ELSE 

ALL : "library - Win32 Release" "$(OUTDIR)\datetest1.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"library - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\datetest1.obj"
	-@erase "$(OUTDIR)\datetest1.exe"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /warn:nofileopt\
 /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\datetest1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\datetest1.pdb" /machine:I386 /out:"$(OUTDIR)\datetest1.exe" 
LINK32_OBJS= \
	"$(INTDIR)\datetest1.obj" \
	"$(OUTDIR)\libts.lib"

"$(OUTDIR)\datetest1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "datetest1 - Win32 Debug"

OUTDIR=.\DEBUG
INTDIR=.\DEBUG
# Begin Custom Macros
OutDir=.\DEBUG
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\datetest1.exe" "$(OUTDIR)\DF50.PDB"

!ELSE 

ALL : "library - Win32 Debug" "$(OUTDIR)\datetest1.exe" "$(OUTDIR)\DF50.PDB"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"library - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\datetest1.obj"
	-@erase "$(INTDIR)\DF50.PDB"
	-@erase "$(OUTDIR)\datetest1.exe"
	-@erase "$(OUTDIR)\datetest1.ilk"
	-@erase "$(OUTDIR)\datetest1.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /debug:full /optimize:0\
 /warn:nofileopt /module:"DEBUG/" /object:"DEBUG/" /pdbfile:"DEBUG/DF50.PDB" 
F90_OBJS=.\DEBUG/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\datetest1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\datetest1.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)\datetest1.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\datetest1.obj" \
	".\libts.lib"

"$(OUTDIR)\datetest1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(CFG)" == "datetest1 - Win32 Release" || "$(CFG)" ==\
 "datetest1 - Win32 Debug"

!IF  "$(CFG)" == "datetest1 - Win32 Release"

"library - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\library.mak CFG="library - Win32 Release" 
   cd "."

"library - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\library.mak CFG="library - Win32 Release"\
 RECURSE=1 
   cd "."

!ELSEIF  "$(CFG)" == "datetest1 - Win32 Debug"

"library - Win32 Debug" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\library.mak CFG="library - Win32 Debug" 
   cd "."

"library - Win32 DebugCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\library.mak CFG="library - Win32 Debug"\
 RECURSE=1 
   cd "."

!ENDIF 

SOURCE=.\datetest1.f90

!IF  "$(CFG)" == "datetest1 - Win32 Release"

DEP_F90_DATET=\
	".\Release\date_time.mod"\
	".\Release\utility.mod"\
	

"$(INTDIR)\datetest1.obj" : $(SOURCE) $(DEP_F90_DATET) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "datetest1 - Win32 Debug"

DEP_F90_DATET=\
	".\Debug\date_time.mod"\
	".\Debug\utility.mod"\
	

"$(INTDIR)\datetest1.obj" : $(SOURCE) $(DEP_F90_DATET) "$(INTDIR)"


!ENDIF 


!ENDIF 

