# Microsoft Developer Studio Project File - Name="datetest1" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=datetest1 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "datetest1.mak".
!MESSAGE 
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

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "datetest1 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "datetest"
# PROP BASE Intermediate_Dir "datetest"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"datetest/" /compile_only /nologo /warn:nofileopt
# ADD F90 /include:"Release/" /compile_only /nologo /warn:nofileopt
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "datetest1 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "datetes0"
# PROP BASE Intermediate_Dir "datetes0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "DEBUG"
# PROP Intermediate_Dir "DEBUG"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"datetes0/" /compile_only /nologo /debug:full /optimize:0 /warn:nofileopt
# ADD F90 /include:"DEBUG/" /compile_only /nologo /debug:full /optimize:0 /warn:nofileopt
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "datetest1 - Win32 Release"
# Name "datetest1 - Win32 Debug"
# Begin Source File

SOURCE=.\datetest1.f90

!IF  "$(CFG)" == "datetest1 - Win32 Release"

NODEP_F90_DATET=\
	".\Release\date_time.mod"\
	".\Release\utility.mod"\
	

!ELSEIF  "$(CFG)" == "datetest1 - Win32 Debug"

DEP_F90_DATET=\
	".\Debug\date_time.mod"\
	".\Debug\utility.mod"\
	

!ENDIF 

# End Source File
# End Target
# End Project
