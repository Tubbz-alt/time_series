AM_FFLAGS = $(FCFLAGS)

EXTRA_DIST = \
	README \
	Makefile.noauto \
	fptrap-intel.f90 \
	fptraptest.f90 \
    fptrap-null.f90  \
	fptraptest.dsp \
	fptrap-win32.f90


lib_LIBRARIES = libts.a

libts_a_SOURCES = \
	time_series.f90 \
	julian.f90 \
	date_time_module.f90 \
	utility.f90

libts_a_LIBADD = @F90TRAPOBJ@
libts_a_DEPENDENCIES = @F90TRAPOBJ@

check_PROGRAMS = \
	datetest1 \
	datetest2 \
	tstest1 \
	tstest2 \
	tstest3 \
	fptraptest

datetest1_SOURCES = datetest1.f90
datetest1_LDADD = libts.a 
datetest2_SOURCES = datetest2.f90
datetest2_LDADD = libts.a 
tstest1_SOURCES = tstest1.f90
tstest1_LDADD = libts.a
tstest2_SOURCES = tstest2.f90
tstest2_LDADD = libts.a
tstest3_SOURCES = tstest3.f90
tstest3_LDADD = libts.a
fptraptest_SOURCES = fptraptest.f90
fptraptest_LDADD = libts.a

# dependancies for individual object files

tstest1.o: tstest1.f90 time_series.o
tstest2.o: tstest2.f90 time_series.o
tstest3.o: tstest3.f90 time_series.o
datetime1.o: datetime1.f90 date_time_module.o utility.o
time_series.o: time_series.f90 date_time_module.o utility.o
date_time_module.o: date_time_module.f90 julian.o
julian.o: julian.f90
utility.o: utility.f90
fptrap-null.o: fptrap-null.f90 utility.o
fptrap-intel.o: fptrap-intel.f90 utility.o

clean-local:
	rm -f *.$(F90MODULEEXT)
	rm -f fort.*
	rm -f *~
