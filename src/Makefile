
WARN_FLAGS = -Wall
# LLVM_FLAGS = -fllvm -optlo-O3
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) -rtsopts -H512m -O2 -threaded

# By default, we build
.PHONY : all
all: build

# Profile build
# Run with '+RTS -p -s' or '+RTS -xc'
.PHONY: profile
profile: PROF_FLAGS = -prof -fprof-auto -caf-all -fforce-recomp
profile: build

# Syntax only, no binary output
.PHONY : check
check:
	ghc $(GHC_FLAGS) -fno-code --make Main

# Build the full program
.PHONY : build
build:
	ghc $(GHC_FLAGS) -o out --make Main

HSSRC = $(wildcard *.hs)
.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) out
	$(RM) log/*.json trace.log
	$(RM) -r ~/.http_img_cache/

