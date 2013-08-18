all: test
.PHONY: all test clean clobber

test: src/Main.exe
	./$<

src/Main.exe: src/Main.hs src/Board.hs src/Game.hs src/TicTacTop.hs
	ghc --make $^ -o "$@"


clean:
	rm src/*.hi src/*.o

clobber: clean
	rm src/*.exe
