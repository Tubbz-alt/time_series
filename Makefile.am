EXTRA_DIST = \
	README \
	Makefile.noauto


lib_LIBRARIES = libts.a

libts_a_SOURCES = \
	time_series.f90 \
	julian.f90 \
	date_time_module.f90 \
	utility.f90

bin_PROGRAMS = \
	datetest1 \
	tstest1 \
	tstest2 

datetest1_SOURCES = datetest1.f90
datetest1_LDADD = libts.a 
tstest1_SOURCES = tstest1.f90
tstest1_LDADD = libts.a
tstest2_SOURCES = tstest2.f90
tstest2_LDADD = libts.a

# dependancies for individual object files

tstest1.o: tstest1.f90 time_series.o
tstest2.o: tstest2.f90 time_series.o
datetime1.o: datetime1.f90 date_time_module.o utility.o
time_series.o: time_series.f90 date_time_module.o utility.o
date_time_module.o: date_time_module.f90 julian.o
julian.o: julian.f90
utility.o: utility.f90

clean-local:
	rm -f *.$(F90MODULEEXT)
	rm -f fort.*
	rm -f *~
