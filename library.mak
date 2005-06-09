# Microsoft Developer Studio Generated NMAKE File, Based on library.dsp
!IF "$(CFG)" == ""
CFG=library - Win32 Debug
!MESSAGE No configuration specified. Defaulting to library - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "library - Win32 Release" && "$(CFG)" !=\
 "library - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "library.mak" CFG="library - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "library - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "library - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

F90=df.exe

!IF  "$(CFG)" == "library - Win32 Release"

OUTDIR=.
INTDIR=.\Release
# Begin Custom Macros
OutDir=.
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\libts.lib" "$(OUTDIR)\Release\time_series.mod"\
 "$(OUTDIR)\Release\fptrap.mod"

!ELSE 

ALL : "$(OUTDIR)\libts.lib" "$(OUTDIR)\Release\time_series.mod"\
 "$(OUTDIR)\Release\fptrap.mod"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\date_time.mod"
	-@erase "$(INTDIR)\date_time_module.obj"
	-@erase "$(INTDIR)\fptrap-win32.obj"
	-@erase "$(INTDIR)\fptrap.mod"
	-@erase "$(INTDIR)\julian.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\time_series.mod"
	-@erase "$(INTDIR)\time_series.obj"
	-@erase "$(INTDIR)\utility.mod"
	-@erase "$(INTDIR)\utility.obj"
	-@erase "$(OUTDIR)\libts.lib"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /optimize:3\
 /warn:nofileopt /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\library.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libts.lib" 
LIB32_OBJS= \
	"$(INTDIR)\date_time_module.obj" \
	"$(INTDIR)\fptrap-win32.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\time_series.obj" \
	"$(INTDIR)\utility.obj"

"$(OUTDIR)\libts.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "library - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : ".\libts.lib" "$(OUTDIR)\time_series.mod" "$(OUTDIR)\fptrap.mod"

!ELSE 

ALL : ".\libts.lib" "$(OUTDIR)\time_series.mod" "$(OUTDIR)\fptrap.mod"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\date_time.mod"
	-@erase "$(INTDIR)\date_time_module.obj"
	-@erase "$(INTDIR)\fptrap-win32.obj"
	-@erase "$(INTDIR)\fptrap.mod"
	-@erase "$(INTDIR)\JULIAN.mod"
	-@erase "$(INTDIR)\julian.obj"
	-@erase "$(INTDIR)\time_series.mod"
	-@erase "$(INTDIR)\time_series.obj"
	-@erase "$(INTDIR)\utility.mod"
	-@erase "$(INTDIR)\utility.obj"
	-@erase ".\libts.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/include:"$(INTDIR)\\" /compile_only /nologo /debug:full /optimize:0\
 /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF50.PDB" 
F90_OBJS=.\Debug/
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\library.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"libts.lib" 
LIB32_OBJS= \
	"$(INTDIR)\date_time_module.obj" \
	"$(INTDIR)\fptrap-win32.obj" \
	"$(INTDIR)\julian.obj" \
	"$(INTDIR)\time_series.obj" \
	"$(INTDIR)\utility.obj"

".\libts.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
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


!IF "$(CFG)" == "library - Win32 Release" || "$(CFG)" ==\
 "library - Win32 Debug"
SOURCE=.\date_time_module.f90

!IF  "$(CFG)" == "library - Win32 Release"

DEP_F90_DATE_=\
	".\Release\julian.mod"\
	
F90_MODOUT=\
	"date_time"


"$(INTDIR)\date_time_module.obj"	"$(INTDIR)\date_time.mod" : $(SOURCE)\
 $(DEP_F90_DATE_) "$(INTDIR)" "$(INTDIR)\julian.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "library - Win32 Debug"

DEP_F90_DATE_=\
	".\Debug\JULIAN.mod"\
	
F90_MODOUT=\
	"date_time"


"$(INTDIR)\date_time_module.obj"	"$(INTDIR)\date_time.mod" : $(SOURCE)\
 $(DEP_F90_DATE_) "$(INTDIR)" "$(INTDIR)\JULIAN.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=".\fptrap-win32.f90"

!IF  "$(CFG)" == "library - Win32 Release"

DEP_F90_FPTRA=\
	".\Release\utility.mod"\
	
F90_MODOUT=\
	"fptrap"


"$(INTDIR)\fptrap-win32.obj"	"$(INTDIR)\fptrap.mod" : $(SOURCE)\
 $(DEP_F90_FPTRA) "$(INTDIR)" "$(INTDIR)\utility.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "library - Win32 Debug"

DEP_F90_FPTRA=\
	".\Debug\utility.mod"\
	
F90_MODOUT=\
	"fptrap"


"$(INTDIR)\fptrap-win32.obj"	"$(INTDIR)\fptrap.mod" : $(SOURCE)\
 $(DEP_F90_FPTRA) "$(INTDIR)" "$(INTDIR)\utility.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\julian.f90

!IF  "$(CFG)" == "library - Win32 Release"

F90_MODOUT=\
	"julian"


"$(INTDIR)\julian.obj"	"$(INTDIR)\julian.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "library - Win32 Debug"

F90_MODOUT=\
	"julian"


"$(INTDIR)\julian.obj"	"$(INTDIR)\JULIAN.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\time_series.f90

!IF  "$(CFG)" == "library - Win32 Release"

DEP_F90_TIME_=\
	".\Release\date_time.mod"\
	".\Release\utility.mod"\
	
F90_MODOUT=\
	"time_series"


"$(INTDIR)\time_series.obj"	"$(INTDIR)\time_series.mod" : $(SOURCE)\
 $(DEP_F90_TIME_) "$(INTDIR)" "$(INTDIR)\utility.mod" "$(INTDIR)\date_time.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "library - Win32 Debug"

DEP_F90_TIME_=\
	".\Debug\date_time.mod"\
	".\Debug\utility.mod"\
	
F90_MODOUT=\
	"time_series"


"$(INTDIR)\time_series.obj"	"$(INTDIR)\time_series.mod" : $(SOURCE)\
 $(DEP_F90_TIME_) "$(INTDIR)" "$(INTDIR)\date_time.mod" "$(INTDIR)\utility.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\utility.f90

!IF  "$(CFG)" == "library - Win32 Release"

F90_MODOUT=\
	"utility"


"$(INTDIR)\utility.obj"	"$(INTDIR)\utility.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "library - Win32 Debug"

F90_MODOUT=\
	"utility"


"$(INTDIR)\utility.obj"	"$(INTDIR)\utility.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

