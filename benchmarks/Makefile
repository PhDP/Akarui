ghc := ghc
ghcflags := -O3

binaries := ExactInference

all: $(binaries)

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ $^

clean:
	-rm -f *.o *.hi $(binaries) $(binaries:%=%_p)

