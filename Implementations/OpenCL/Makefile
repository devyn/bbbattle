CC=gcc
LD=$(CC)
CFLAGS=
EXTRA_CFLAGS=
LDFLAGS=
EXTRA_LDFLAGS=
STATIC_OBJECTS=

ifdef MINGW32_AMD
	WINDOWS=1
	EXTRA_CFLAGS   +=-I"/c/Program Files (x86)/AMD APP/include"
	STATIC_OBJECTS +="/c/Program Files (x86)/AMD APP/lib/x86/libOpenCL.a"
endif
ifdef MINGW64_AMD
	WINDOWS=1
	EXTRA_CFLAGS   +=-I"/c/Program Files (x86)/AMD APP/include"
	STATIC_OBJECTS +="/c/Program Files (x86)/AMD APP/lib/x86_64/libOpenCL.a"
endif

ifeq ($(shell uname), Darwin)
	MACOSX=1
	CC=clang
	EXTRA_LDFLAGS +=-framework OpenCL
else
	ifndef WINDOWS
		EXTRA_LDFLAGS +=-lOpenCL
	endif
endif

ifdef WINDOWS
	EXECUTABLE_NAME=bbbattle.exe
else
	EXECUTABLE_NAME=bbbattle
endif

OBJECTS=bbbattle.o fmt_bbbattle.o fmt_bbbout.o

all: $(EXECUTABLE_NAME)

$(EXECUTABLE_NAME): $(OBJECTS)
	$(LD) $(EXTRA_LDFLAGS) $(LDFLAGS) -o $@ $(OBJECTS) $(STATIC_OBJECTS)

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
