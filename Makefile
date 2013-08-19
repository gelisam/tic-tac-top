all: test
.PHONY: all test clean clobber

test: src/Main.exe
	./$< tic-tac-top

src/Main.exe: src/Player.hs src/Board.hs src/Game.hs src/AI.hs src/TicTacTop.hs src/TicTacTop2.hs src/Play.hs src/Main.hs
	ghc --make $^ -o "$@"


clean:
	rm src/*.hi src/*.o

clobber: clean
	rm src/*.exe
