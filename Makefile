all: test
.PHONY: all test clean clobber

test: src/Main.exe
	./$<

src/Main.exe: src/Main.hs
	ghc --make $<


clean:
	rm src/*.hi src/*.o

clobber: clean
	rm src/*.exe
