#GFORTRAN OS X ###
FC = gfortran
FFLAGS = -m64 -fdefault-real-8
TARGET_ARCH =
LDFLAGS = -m64
LIBS = 

EXE   = StrGrid
VPATH = mod

.SUFFIXES: .f90 .o

GRAFIC_HEADER =

SRCMOD =    			\
	AlgModule.f90		\
	Structures.f90		\
	solveLaplace.f90	\
	paraview.f90

SRCMAIN =      			\
	StrGrid.f90

OBJMOD = ${SRCMOD:.f90=.o}

OBJMAIN = ${SRCMAIN:.f90=.o}

OBJ = $(OBJMOD) $(OBJMAIN)

$(EXE): $(OBJ)
	$(FC) $(LDFLAGS) $(OBJ) $(LIBS) -o $(EXE)

%.o  : %.f90
	$(FC) $(FFLAGS) -c $<

# Define dependencies for modules
$(OBJMAIN): $(OBJMOD)

# clean any generated files
clean:
	-rm -f *.o *~ core

