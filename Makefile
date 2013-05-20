HC=ghc
HCFLAGS=-threaded -rtsopts -with-rtsopts="-N -qg0 -H64m" -O2
SOURCES=brain-battle.hs

all: brain-battle

brain-battle: $(SOURCES)
	$(HC) $(HCFLAGS) -o brain-battle $(SOURCES)

clean:
	rm -f brain-battle *.o *.hi

.PHONY: clean all
