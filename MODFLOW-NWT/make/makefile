# makefile created by pymake (version 1.2.5) for the 'mfnwt' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src

VPATH = \
${SOURCEDIR1} 

.SUFFIXES: .f90 .F90 .f .fpp .o

OBJECTS = \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/openspec.o \
$(OBJDIR)/gwflakmodule_NWT.o \
$(OBJDIR)/gwf2bas7_NWT.o \
$(OBJDIR)/gwf2huf7.o \
$(OBJDIR)/gwfsfrmodule_NWT.o \
$(OBJDIR)/gwf2bcf7.o \
$(OBJDIR)/sip7_NWT.o \
$(OBJDIR)/gwf2str7.o \
$(OBJDIR)/gwfuzfmodule_NWT.o \
$(OBJDIR)/gwf2chd7.o \
$(OBJDIR)/gwf2ghb7_NWT.o \
$(OBJDIR)/gwf2ibs7.o \
$(OBJDIR)/gwf2sub7_NWT.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/gwf2drn7_NWT.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/gwf2drt7.o \
$(OBJDIR)/obs2str7.o \
$(OBJDIR)/gwf2ets7.o \
$(OBJDIR)/gwf2swr7util.o \
$(OBJDIR)/gwf2evt7.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/gwf2swt7.o \
$(OBJDIR)/gwf2fhb7.o \
$(OBJDIR)/pcg7_NWT.o \
$(OBJDIR)/gwf2gag7.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/gwf2hydmod7.o \
$(OBJDIR)/hufutl7.o \
$(OBJDIR)/gwf2lpf7.o \
$(OBJDIR)/mhc7.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/modules.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/de47_NWT.o \
$(OBJDIR)/gwf2rch7.o \
$(OBJDIR)/nogmg.o \
$(OBJDIR)/gwf2res7.o \
$(OBJDIR)/gwf2riv7_NWT.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/gwf2upw1.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/gwf2lak7_NWT.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/MF_NWT.o \
$(OBJDIR)/gwf2mnw17_NWT.o \
$(OBJDIR)/gwf2mnw27_NWT.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/gwf2ag1_NWT_mf.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/gwf2sfr7_NWT.o \
$(OBJDIR)/gwf2swi27.o \
$(OBJDIR)/gwf2swr7.o \
$(OBJDIR)/gwf2uzf1_NWT.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/lmt8_NWT.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

