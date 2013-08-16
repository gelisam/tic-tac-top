module Board where


type Player = Bool
type PlayerIx = Int

player_index :: Player -> PlayerIx
player_index p = if p then 1 else 2

indexed_player :: PlayerIx -> Player
indexed_player 1 = True
indexed_player 2 = False


type Cell = Maybe Player
type CellIx = Int

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

at :: Board -> Pos -> Cell
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
