CC=gcc
LD=$(CC)
CFLAGS=

ifeq ($(shell uname), Darwin)
	MACOSX=1
	LDFLAGS=-framework OpenCL
else
	LDFLAGS=-lOpenCL
endif

ifdef MINGW32_AMD
	CFLAGS  += -I"/c/Program Files (x86)/AMD APP/include"
	LDFLAGS += -L"/c/Program Files (x86)/AMD APP/bin/x86"
endif
ifdef MINGW64_AMD
	CFLAGS  += -I"/c/Program Files (x86)/AMD APP/include"
	LDFLAGS += -L"/c/Program Files (x86)/AMD APP/bin/x86_64"
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
