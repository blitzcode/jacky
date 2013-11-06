
# Main Makefile for Jacky

WARN_FLAGS = -Wall
# LLVM_FLAGS = -fllvm -optlo-O3
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) -rtsopts -H512m -O2 -threaded

# By default, we build
.PHONY : all
all: jacky

HSSRC = $(wildcard *.hs)

# We assume all .cpp files in the directory are meant to be compiled
CXXSRC = $(wildcard *.cpp)
CXXOBJ = $(CXXSRC:.cpp=.o)

# Profile build
# Run with '+RTS -p -s' or '+RTS -xc'
.PHONY: profile
profile: PROF_FLAGS = -prof -fprof-auto -caf-all -fforce-recomp
profile: jacky

# Build the full program
jacky: $(HSSRC) $(CXXOBJ)
	ghc $(GHC_FLAGS) -o $@ -lstdc++ --make Main $(CXXOBJ)

.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) jacky
	$(RM) $(CXXOBJ) $(CXXOBJ:.o=.d)
	$(RM) log/*.json trace.log
	$(RM) -r ~/.http_img_cache/

#
# C++ compilation rules beyond this point
#

# Clang for the C++ sources
CXX = clang++
CXXFLAGS = -std=c++11 -g -m32 -O3 -mtune=native -Wall -Wextra -Wno-unused-parameter

# Only include this if we actually build the main program, don't want to
# generate dependencies for the clean targets etc.
ifeq ($(MAKECMDGOALS), )

# Automatically generate dependencies with the compiler for each C++ source file
%.d: %.cpp
	@set -e; rm -f $@; \
	 $(CXX) -MM $(CXXFLAGS) $< > $@.$$$$; \
	 sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	 $(RM) $@.$$$$

# Include the generated dependencies for object and dependency files. Be silent
# during the first compile where the .d files have not been generated yet and
# everything is recompiled
-include $(CXXSRC:.cpp=.d)

endif

