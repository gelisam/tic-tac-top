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


data GameState = GameState
  { minRow :: Int
  , player :: Player
  , board :: Board
  }


type Coord = (Int, Int)

at :: Board -> Coord -> Cell
at b (x, y) = b !! y !! x


winner :: Board -> Maybe Player
winner b = h_loser
       <|> v_winner
       <|> d1_winner
       <|> d2_winner
  where
    range = [0..2]
    h_loser = fmap not h_winner
    h_winner  = lookup_winners [[(x,  y) | x <- range] | y <- range]
    v_winner  = lookup_winners [[(x,  y) | y <- range] | x <- range]
    d1_winner = lookup_winners [[(i,  i) | i <- range]]
    d2_winner = lookup_winners [[(i,2-i) | i <- range]]
    
    lookup_winners :: [[Coord]] -> Maybe Player
    lookup_winners = msum . map lookup_winner
    
    lookup_winner :: [Coord] -> Maybe Player
    lookup_winner = list_winner . map (b `at`)
    
    list_winner xs = case nub xs of
      [maybe_p] -> maybe_p
      _         -> Nothing


main = do b <- readBoard <$> getContents
          print $ printCell $ winner b
