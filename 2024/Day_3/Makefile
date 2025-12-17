# Compiler
FC = gfortran
# Compiler flags
FFLAGS = -O2 -Wall -g -ffree-line-length-none

# Source and object files
SRCS = main.f90 #levels_mod.f90 main.f90
OBJS = $(SRCS:.f90=.o)

# Target executable
EXEC = main

# Default rule
all: $(EXEC)

# Link the executable
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^

# Generic rule for compiling .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Explicit dependency: Ensures all non-main .f90 files are compiled before main.f90
#main.o: levels_mod.o

# Clean rule
clean:
	rm -f $(OBJS) $(EXEC) *.mod
