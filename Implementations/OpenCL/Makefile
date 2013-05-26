CC=gcc
LD=$(CC)
CFLAGS=
EXTRA_CFLAGS=
LDFLAGS=
EXTRA_LDFLAGS=

ifeq ($(shell uname), Darwin)
	MACOSX=1
	CC=clang
	EXTRA_LDFLAGS +=-framework OpenCL
else
	EXTRA_LDFLAGS +=-lOpenCL
endif

ifdef MINGW32_AMD
	WINDOWS=1
	EXTRA_CFLAGS  +=-I"/c/Program Files (x86)/AMD APP/include"
	EXTRA_LDFLAGS +=-L"/c/Program Files (x86)/AMD APP/bin/x86"
endif
ifdef MINGW64_AMD
	WINDOWS=1
	EXTRA_CFLAGS  +=-I"/c/Program Files (x86)/AMD APP/include"
	EXTRA_LDFLAGS +=-L"/c/Program Files (x86)/AMD APP/bin/x86_64"
endif

ifdef WINDOWS
	EXECUTABLE_NAME=bbbattle.exe
else
	EXECUTABLE_NAME=bbbattle
endif

OBJECTS=bbbattle.o fmt_bbbattle.o fmt_bbbout.o

all: $(EXECUTABLE_NAME)

$(EXECUTABLE_NAME): $(OBJECTS)
	$(LD) $(EXTRA_LDFLAGS) $(LDFLAGS) -o $@ $(OBJECTS)

bbbattle.o: bbbattle.c bbbattle.cl.h
	$(CC) $(EXTRA_CFLAGS) $(CFLAGS) -c -o $@ bbbattle.c

bbbattle.cl.h: bbbattle.cl
	ruby kernel_to_header.rb bbbattle.cl

%.o: %.c
	$(CC) $(EXTRA_CFLAGS) $(CFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f bbbattle.cl.h
	rm -f $(OBJECTS)
	rm -f $(EXECUTABLE_NAME)
