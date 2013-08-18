-- a standard 3x3 tic-tac-toe board.
-- the small size of the board allows the AI to quickly explore the entire
-- state space, which in turn allows us to find variants of tic-tac-toe which
-- don't always end in a tie.
module Board where

import Player


type Cell = Maybe Player
type CellIx = Int

cell_range :: (CellIx, CellIx)
cell_range = (0,2)

cell_index :: Cell -> CellIx
cell_index Nothing = 0
cell_index (Just x) = player_index x

indexed_cell :: CellIx -> Cell
indexed_cell 0 = Nothing
indexed_cell x = Just (indexed_player x)


type Board = [[Cell]]
type BoardIx = (CellIx,(CellIx,(CellIx,
               (CellIx,(CellIx,(CellIx,
               (CellIx,(CellIx,(CellIx)))))))))

board_range :: (BoardIx, BoardIx)
board_range = (board9 empty, board9 full)
  where
    empty = fst cell_range
    full  = snd cell_range
    board9 = board_index . replicate9 . indexed_cell
    replicate9 = replicate 3 . replicate 3

board_index :: Board -> BoardIx
board_index = go . (map.map) cell_index
  where
    go [[a,b,c],[d,e,f],[g,h,i]] = (a,(b,(c,(d,(e,(f,(g,(h,i))))))))

indexed_board :: BoardIx -> Board
indexed_board = (map.map) indexed_cell . go
  where
    go (a,(b,(c,(d,(e,(f,(g,(h,i)))))))) = [[a,b,c],[d,e,f],[g,h,i]]


readPlayer :: Char -> Player
readPlayer 'x' = True
readPlayer 'o' = False

readCell :: Char -> Cell
readCell '.' = Nothing
readCell x = Just $ readPlayer x

readBoard :: String -> Board
readBoard = (map.map) readCell . lines


printPlayer :: Player -> Char
printPlayer True  = 'x'
printPlayer False = 'o'

printCell :: Cell -> Char
printCell Nothing  = '.'
printCell (Just x) = printPlayer x

printBoard :: Board -> String
printBoard = unlines . (map.map) printCell


type Pos = (Int, Int)

at :: [[a]] -> Pos -> a
at b (x, y) = b !! y !! x

set_at :: Pos -> Cell -> Board -> Board
set_at (x, y) c b = top
                 ++ [left ++ [c] ++ right]
                 ++ bottom
  where
    (top, row:bottom) = splitAt y b
    (left,  _:right)  = splitAt x row

pos_range :: [Int]
pos_range = [0..2]

positions = [(x, y) | x <- pos_range
                    , y <- pos_range
                    ]
