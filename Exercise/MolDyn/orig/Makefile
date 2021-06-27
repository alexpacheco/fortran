ifeq ($(COMP),intel)
	FC=ifort
	FFLAGS=
else ifeq ($(COMP),pgi)
	FC=pgif90
	FFLAGS=
else ifeq ($(COMP),ibm)
	FC=xlf90
	FFLAGS=
else
	FC=gfortran-mp-11
	FFLAGS=-g  -pg
endif

OBJS = md-orig.o

all : serial

serial : $(OBJS)
	$(FC) $(FFLAGS) -o md $(OBJS)

runmd : md
	./md 

%.o :%.f90
	$(FC) $(FFLAGS) -c $<
