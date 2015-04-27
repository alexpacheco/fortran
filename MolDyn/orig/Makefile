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
	FC=gfortran
	FFLAGS=
endif

OBJS = md-orig.o

all : serial

serial : $(OBJS)
	$(FC) $(FFLAGS) -o md $(OBJS)

%.o :%.f90
	$(FC) $(FFLAGS) -c $<
