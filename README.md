Tic-tac-top
===

A console-based version of [Tic-tac-top](http://gelisam.com/board/tic-tac-top/).

Unlike the online version linked above, the console version has an AI against which you can play. Here is an annotated match.

    > ./src/Main.exe
    please wait while the computer evaluates your chances.

    ...
    ...
    ...

    You could win in 3 moves.

Since the game is played on such a tiny board, the computer can explore the entire state space in a few seconds, and reveals that the first player has a strategy for a guaranteed win. Luckily, that's you!

    123
    456
    789
    > 1
    
    o..
    ...
    ...

You pick the top-left corner, because why not.

    o.. (forbidden)
    ...
    ...

    o..
    .x.
    ...

The computer picks the central position. Note that by playing in the first row, you are preventing the computer from playing on that row on this turn.

    o.. (forbidden)
    .x. (forbidden)
    789
    > 9
    
    o..
    .x.
    ..o
    
    I think I can win in 2 moves.
    
    o.x
    .x.
    ..o

You picked another corner, but the computer doesn't think that was a very smart move. If you are not careful, it will form a diagonal line soon!

    o.x (forbidden)
    4x6
    78o
    > 7
    
    o.x
    .x.
    o.o

You deftly block the computer's diagonal. The full rules for forbidden rows is that if you can play on one of the rows below your opponent's, then you must.

    I think I can win in 1 moves.
    
    o.x
    .xx
    o.o
    
    Checkmates!

Since you played on the last row, and there are no rows after that, the computer is allowed to play anywhere. It used the opportunity to trap you into a corner!

    o.x (forbidden)
    .xx (forbidden)
    o8o
    > 8
    
    o.x
    .xx
    ooo
    
    The computer wins.

By playing on the middle row, the computer is forcing you to complete a horizontal three-in-a-row, which are deadly in tic-tac-top. All other kinds of three-in-a-rows are wins, but horizontal three-in-a-rows are wins for your *opponent*.

Oh well. How about a rematch?

Installation
---

The game isn't installed anywhere, `make` will compile it inside the current folder and play it there. The AI is written in Haskell, which is the only dependency.

    > make
    ghc --make src/Main.hs src/Board.hs -o "src/Main.exe"
    [1 of 2] Compiling Board            ( src/Board.hs, src/Board.o )
    [2 of 2] Compiling Main             ( src/Main.hs, src/Main.o )
    Linking src/Main.exe ...
    ./src/Main.exe
    please wait while the computer evaluates your chances.

    ...
    ...
    ...

    You could win in 3 moves.

Notice it says you *could*. Will you?
