module Main where

import Control.Applicative
import Control.Monad
import Data.List


type Player = Bool
type Cell = Maybe Player
type Board = [[Cell]]


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


-- main = do b <- readBoard <$> getContents
--           print $ printCell $ winner b
main = putStrLn "typechecks."
