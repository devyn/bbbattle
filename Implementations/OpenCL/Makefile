CC=gcc
LD=$(CC)
CFLAGS=

ifeq ($(shell uname), Darwin)
	MACOSX=1
	LDFLAGS=-framework OpenCL
else
	MACOSX=0
	LDFLAGS=-lOpenCL
endif

OBJECTS=bbbattle.o

all: bbbattle

bbbattle: $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $<

bbbattle.o: bbbattle.c bbbattle.cl.h
	$(CC) $(CFLAGS) -c -o $@ $<

bbbattle.cl.h: bbbattle.cl
	ruby kernel_to_header.rb bbbattle.cl

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f bbbattle.cl.h
	rm -f $(OBJECTS)
	rm -f bbbattle
