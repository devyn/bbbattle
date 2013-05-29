CC=gcc
LD=$(CC)
CFLAGS=
LDFLAGS=

BBBATTLE_CFLAGS=
BBBATTLE_LDFLAGS=
BBBATTLE_OBJECTS=bbbattle.o fmt_bbbattle.o fmt_bbbout_write.o fmt_bbbout_common.o
BBBATTLE_STATIC_OBJECTS=

BBBOUT_SDL_CFLAGS=$(shell sdl-config --cflags)
BBBOUT_SDL_LDFLAGS=$(shell sdl-config --libs)
BBBOUT_SDL_OBJECTS=bbbout_sdl.o fmt_bbbout_read.o fmt_bbbout_common.o
BBBOUT_SDL_STATIC_OBJECTS=

ifdef MINGW32_AMD
	WINDOWS=1
	BBBATTLE_CFLAGS         +=-I"/c/Program Files (x86)/AMD APP/include"
	BBBATTLE_STATIC_OBJECTS +="/c/Program Files (x86)/AMD APP/lib/x86/libOpenCL.a"

	BBBOUT_SDL_STATIC_OBJECTS +=/usr/lib/libSDL.dll.a
endif
ifdef MINGW64_AMD
	WINDOWS=1
	BBBATTLE_CFLAGS         +=-I"/c/Program Files (x86)/AMD APP/include"
	BBBATTLE_STATIC_OBJECTS +="/c/Program Files (x86)/AMD APP/lib/x86_64/libOpenCL.a"

	BBBOUT_SDL_STATIC_OBJECTS +=/usr/lib/libSDL.dll.a
endif

ifeq ($(shell uname), Darwin)
	MACOSX=1
	CC=clang
	BBBATTLE_LDFLAGS +=-framework OpenCL
else
	ifndef WINDOWS
		BBBATTLE_LDFLAGS +=-lOpenCL
	endif
endif

ifdef WINDOWS
	EXECUTABLE_SUFFIX=.exe
else
	EXECUTABLE_SUFFIX=
endif

all: bbbattle$(EXECUTABLE_SUFFIX) bbbout_sdl$(EXECUTABLE_SUFFIX)

bbbattle$(EXECUTABLE_SUFFIX): $(BBBATTLE_OBJECTS)
	$(LD) $(BBBATTLE_LDFLAGS) $(LDFLAGS) -o $@ $(BBBATTLE_OBJECTS) $(BBBATTLE_STATIC_OBJECTS)

bbbattle.o: bbbattle.c bbbattle.cl.h
	$(CC) $(BBBATTLE_CFLAGS) $(CFLAGS) -c -o $@ bbbattle.c

bbbattle.cl.h: bbbattle.cl
	ruby kernel_to_header.rb bbbattle.cl

bbbout_sdl$(EXECUTABLE_SUFFIX): $(BBBOUT_SDL_OBJECTS)
	$(LD) $(BBBOUT_SDL_LDFLAGS) $(LDFLAGS) -o $@ $(BBBOUT_SDL_OBJECTS) $(BBBOUT_SDL_STATIC_OBJECTS)

bbbout_sdl.o: bbbout_sdl.c
	$(CC) $(BBBOUT_SDL_CFLAGS) $(CFLAGS) -c -o $@ bbbout_sdl.c

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f bbbattle.cl.h
	rm -f $(BBBATTLE_OBJECTS)
	rm -f bbbattle$(EXECUTABLE_SUFFIX)
	rm -f $(BBBOUT_SDL_OBJECTS)
	rm -f bbbout_sdl$(EXECUTABLE_SUFFIX)
