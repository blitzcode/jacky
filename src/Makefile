
# We use
# conduit, http-conduit, attoparsec-conduit, aeson, attoparsec, authenticate-oauth,
# glfw-b, JuicyPixels-3.1.1.1

WARN_FLAGS = -Wall
# LLVM_FLAGS = -fllvm -optlo-O3
# Run with '+RTS -p -s' or '+RTS -xc'
# PROF_FLAGS = -prof -fprof-auto
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) -rtsopts -H512m -O2 -threaded

# By default, we build
.PHONY : all
all: build

# Syntax only, no binary output
.PHONY : check
check:
	ghc $(GHC_FLAGS) -fno-code --make Main

# Build the full program
.PHONY : build
build:
	ghc $(GHC_FLAGS) -o out --make Main
	rm -r ~/.http_img_cache/

HSSRC = $(wildcard *.hs)
.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) out

