
FCOMP = gfortran #defaults to gfortran set this to ifort to use intel fortran compiler

BUILDDIR = ${CURDIR}/lib
SRCDIR = ${CURDIR}/src
INCLUDE =  ${CURDIR}/include
OBJDIR = ${CURDIR}/obj


FOBJECTS = ${OBJDIR}/loF_common.o ${OBJDIR}/loF_message.o ${OBJDIR}/loF_address.o ${OBJDIR}/loF_server.o 


LIBS = -llo -lpthread 
IFORTCOMPFLAGS = -fPIC -fast -xHost
IFORTLINKFLAGS = -fast -xHost -shared
GFORTCOMPFLAGS = -fPIC -march=native -ffast-math -funroll-loops -O3
GFORTLINKFLAGS = -O3 -shared


.PHONY: lib 

lib: loF_common loF_message loF_address loF_server
	@echo "Linking"
ifeq ($(FCOMP),ifort)
	@echo "with intel fortran"
	ifort ${IFORTLINKFLAGS} -o ${BUILDDIR}/libloF.so ${FOBJECTS} ${COBJECTS} ${LIBS}
else
	@echo "with gfortran"
	gfortran ${GFORTLINKFLAGS} -o ${BUILDDIR}/libloF.so ${FOBJECTS} ${COBJECTS} ${LIBS}
endif


loF_common:
	@echo "\n++loF common"
ifeq ($(FCOMP),ifort)
	@echo "compiling with intel fortran"
	ifort ${IFORTCOMPFLAGS} -c -Fo${OBJDIR}/ -module ${INCLUDE}/ ${SRCDIR}/loF_common.f90	
else
	@echo "compiling with gfortran"
	gfortran ${GFORTCOMPFLAGS} -c -o${OBJDIR}/loF_common.o -J${INCLUDE}/ ${SRCDIR}/loF_common.f90
endif


loF_message: 
	@echo "\n++loF message"
ifeq ($(FCOMP),ifort)
	@echo "compiling with intel fortran"
	ifort ${IFORTCOMPFLAGS} -c -Fo${OBJDIR}/ -module ${INCLUDE}/ ${SRCDIR}/loF_message.f90	
else
	@echo "compiling with gfortran"
	gfortran ${GFORTCOMPFLAGS} -c -o${OBJDIR}/loF_message.o -J${INCLUDE}/ ${SRCDIR}/loF_message.f90
endif

loF_address: 
	@echo "\n++loF address"
ifeq ($(FCOMP),ifort)
	@echo "compiling with intel fortran"
	ifort ${IFORTCOMPFLAGS} -c -Fo${OBJDIR}/ -module ${INCLUDE}/ ${SRCDIR}/loF_address.f90	
else
	@echo "compiling with gfortran"
	gfortran ${GFORTCOMPFLAGS} -c -o${OBJDIR}/loF_address.o -J${INCLUDE}/ ${SRCDIR}/loF_address.f90
endif

loF_server: 
	@echo "\n++loF server"
ifeq ($(FCOMP),ifort)
	@echo "compiling with intel fortran"
	ifort ${IFORTCOMPFLAGS} -c -Fo${OBJDIR}/ -module ${INCLUDE}/ ${SRCDIR}/loF_server.f90	
else
	@echo "compiling with gfortran"
	gfortran ${GFORTCOMPFLAGS} -c -o${OBJDIR}/loF_server.o -J${INCLUDE}/ ${SRCDIR}/loF_server.f90
endif
