
# Main Makefile for Jacky
#
# In addition to the Haskell code we have a small FreeType 2 wrapper written in
# C as well as the FreeType 2 library itself (C, using its own Makefile). 

APP_NAME = jacky

# By default, we build everything
.PHONY : all
all: $(APP_NAME)

# GHC compiler flags
WARN_FLAGS = -Wall
# LLVM_FLAGS = -fllvm -optlo-O3
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) \
            -rtsopts -with-rtsopts=-T -H512m -O2 -threaded
# TODO: Consider adding -qg1 to the RTS options, seems to be accelerate the
#       program while reducing CPU load (https://ghc.haskell.org/trac/ghc/ticket/8513)

# Profile build
# Run with '+RTS -p -s' or '+RTS -xc'
.PHONY: profile
profile: PROF_FLAGS = -prof -fprof-auto -caf-all -osuf p_o -hisuf p_hi
profile: $(APP_NAME)

# Get some build options out of 'ghc --info'. This is mostly so we can pass
# '-m32' and '-arch i386' to clang/gcc and FT2's configure script in case we're
# running with a 32 bit version of GHC
GHC_CFLAGS  := $(shell ghc --info | ghc -e "fmap read getContents >>= \
               putStrLn . Data.Maybe.fromJust . lookup \"C compiler flags\"")
GHC_LDFLAGS := $(shell ghc --info | ghc -e "fmap read getContents >>= \
               putStrLn . unwords . read . Data.Maybe.fromJust . lookup \"Ld Linker flags\"")

# FT2 static library and libs required by it
FT2_DIR = ./freetype2
FT2_STATIC_LIB = $(FT2_DIR)/objs/.libs/libfreetype.a
FT2_LDFLAGS = -lz -lbz2 -L/usr/X11/lib -lpng12 $(FT2_STATIC_LIB)
# For dynamic linking with FT2
# FT2_LDFLAGS = -L$(FT2_DIR)/objs/.libs -lfreetype

# Rule for building FT2
#
# TODO: It might be cleaner to just compile this fully by ourselves, see
#       $(FT2_DIR)/docs/INSTALL.ANY for a guide
$(FT2_STATIC_LIB):
	cd $(FT2_DIR) && env CFLAGS="$(GHC_LDFLAGS)" ./configure && make -j

# We assume all .c files in the directory are meant to be compiled
CSRC = $(wildcard *.c)
COBJ = $(CSRC:.c=.o)

# All C linking related flags and files
C_LDFLAGS = $(COBJ) $(FT2_LDFLAGS)

HSSRC = $(wildcard *.hs)

# Build the full program
$(APP_NAME): $(HSSRC) $(COBJ) $(FT2_STATIC_LIB)
	ghc $(GHC_FLAGS) -o $@ --make Main $(C_LDFLAGS)

.PHONY : clean
clean:
	cd $(FT2_DIR) && make distclean
	$(RM) $(APP_NAME)
	$(RM) $(APP_NAME).prof $(APP_NAME).aux $(APP_NAME).ps $(APP_NAME).hp
	$(RM) $(HSSRC:.hs=.hi)   $(HSSRC:.hs=.o)
	$(RM) $(HSSRC:.hs=.p_hi) $(HSSRC:.hs=.p_o)
	$(RM) $(COBJ) $(COBJ:.o=.d)
	$(RM) log/*.json trace.log
	$(RM) -r ~/.http_img_cache/

# C compilation rules beyond this point

# Clang for the C sources
CC = clang
CFLAGS = -std=c99 -g -O3 -mtune=native -Wall -Wextra -Wno-unused-parameter \
         -I$(FT2_DIR)/include/ $(GHC_CFLAGS)

# Only include this if we actually build the main program, don't want to
# generate dependencies for the clean targets etc.
ifeq ($(MAKECMDGOALS), )

# Automatically generate dependencies with the compiler for each C source file
%.d: %.c
	@set -e; rm -f $@; \
	 $(CC) -MM $(CFLAGS) $< > $@.$$$$; \
	 sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	 $(RM) $@.$$$$

# Include the generated dependencies for object and dependency files. Be silent
# during the first compile where the .d files have not been generated yet and
# everything is recompiled
-include $(CSRC:.c=.d)

endif

