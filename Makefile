all: test
.PHONY: all test clean clobber

test: src/Main.exe
	./$<

src/Main.exe: src/Main.hs src/Board.hs
	ghc --make $^ -o "$@"


clean:
	rm src/*.hi src/*.o

clobber: clean
	rm src/*.exe
