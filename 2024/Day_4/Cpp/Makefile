# Compiler
CXX = g++

# Compiler flags
# -std=c++17: Use C++17 standard
# -O2: Optimization level 2
# -Wall: Enable all common warnings
# -g: Include debug information
CXXFLAGS = -std=c++17 -O2 -Wall -g

# Source and object files
# List all your .cpp files here
SRCS = functions.cpp main.cpp
OBJS = $(SRCS:.cpp=.o)

# Target executable
EXEC = main

# Default rule
all: $(EXEC)

# Link the executable
$(EXEC): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Generic rule for compiling .cpp to .o
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean rule
clean:
	rm -f $(OBJS) $(EXEC)
