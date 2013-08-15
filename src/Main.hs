module Main where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe


bools :: [Bool]
bools = [True, False]


type Player = Bool

players :: [Player]
players = bools


type Cell = Maybe Player

cells :: [Cell]
cells = Nothing : map Just players


type Board = [[Cell]]

boards :: [Board]
boards = (replicateM 3 . replicateM 3) cells

-- crowded boards first
boards' :: [Board]
boards' = sortBy (compare `on` measures) boards
  where
    measures xs = (count xs, pattern xs, player_string xs)
    count = negate . length . filter isJust . concat
    pattern = map not . map isJust . concat
    player_string = map not . map fromJust . filter isJust . concat

-- crowded boards first
boards'' :: [Board]
boards'' = [split3 cells | n <- [9,8..0]
                         , pattern <- 9 `choose` n
                         , xs <- replicateM n players
                         , let player_string = map Just xs
                         , let blank_string = repeat Nothing
                         , let cells = knit pattern player_string
                                                    blank_string
                         ]
  where
    split3 (a:b:c:d:e:f:g:h:i:[]) = [[a,b,c],[d,e,f],[g,h,i]]
    
    -- knit two strings together into a single strand,
    -- according to the pattern.
    knit :: [Bool] -> [a] -> [a] -> [a]
    knit []         _      _      = []
    knit (True :ps) (t:ts) fs     = t : knit ps ts fs
    knit (False:ps) ts     (f:fs) = f : knit ps ts fs
    
    -- n booleans, k of which are true.
    choose :: Int -> Int -> [[Bool]]
    choose n k | k == 0 = [replicate n False]
    choose n k | k == n = [replicate n True]
    choose n k | otherwise = [x:xs | x <- bools
                                   , xs <- (n-1) `choose` if x then k-1 else k]


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

range :: [Int]
range = [0..2]

positions = [(x, y) | x <- range
                    , y <- range
                    ]


type Winner = Maybe Player

winner :: Board -> Winner
winner b = h_loser
       <|> v_winner
       <|> d1_winner
       <|> d2_winner
  where
    h_loser = fmap not h_winner
    h_winner  = lookup_winners [[(x,  y) | x <- range] | y <- range]
    v_winner  = lookup_winners [[(x,  y) | y <- range] | x <- range]
    d1_winner = lookup_winners [[(i,  i) | i <- range]]
    d2_winner = lookup_winners [[(i,2-i) | i <- range]]
    
    lookup_winners :: [[Pos]] -> Maybe Player
    lookup_winners = msum . map lookup_winner
    
    lookup_winner :: [Pos] -> Maybe Player
    lookup_winner = list_winner . map (b `at`)
    
    list_winner xs = case nub xs of
      [maybe_p] -> maybe_p
      _         -> Nothing



data GameState = GameState
  { minRow :: Int
  , player :: Player
  , board :: Board
  }

type Move = Pos

legal_moves :: GameState -> [Move]
legal_moves g = filter legal positions
  where
    legal p = unoccupied p
           && above_min p
    unoccupied p = (board g `at` p) == Nothing
    above_min p = snd p >= minRow g

play :: Move -> GameState -> GameState
play (x, y) (GameState minRow  player  board) =
             GameState minRow' player' board'
  where
    y' = y + 1
    minRow' = if y' `elem` range then y' else 0
    player' = not player
    board'  = set_at (x, y) (Just player) board

-- all game states, endgames first
-- game_states :: [GameState]
-- game_states = [GameState 0 True | n <- [9,8..0], g <- ]


-- -- survive the longest.
-- -- win if possible, but delay the victory;
-- -- if you must lose, survive as long as possible.
-- -- 
-- -- return the best move, the eventual winner,
-- -- and the number of surviving steps.
-- best_move :: GameState -> (Move, Winner, Int)
-- best_move g = maximumBy (compare `on` value) outcome . legal_moves g


-- main = do b <- readBoard <$> getContents
--           print $ printCell $ winner b
main = do print (sort boards == sort boards') -- True
          print (boards' == boards'')         -- True
