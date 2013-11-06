
# Just forward to the actual Makefile in src

.PHONY: all, clean

all:
	cd src && $(MAKE)

clean:
	cd src && $(MAKE) clean
