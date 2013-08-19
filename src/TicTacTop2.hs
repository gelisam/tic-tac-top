-- a variant of tic-tac-top in which you are allowed to play over
-- an existing piece in the middle (but not twice in a row)
{-# OPTIONS -XTypeFamilies #-}
module TicTacTop2 where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Function
import Data.List

import Player
import Board
import Game
import AI
import Play


data TicTacTop2 = TicTacTop2
  { over :: Bool
  , minRow :: Int
  , player :: Player
  , board :: Board
  }

instance Game TicTacTop2 where
  current_player = player
  
  winner g = h_loser
         <|> v_winner
         <|> d1_winner
         <|> d2_winner
    where
      b = board g
      h_loser = fmap not h_winner
      h_winner  = lookup_winners [[(x,  y) | x <- pos_range] | y <- pos_range]
      v_winner  = lookup_winners [[(x,  y) | y <- pos_range] | x <- pos_range]
      d1_winner = lookup_winners [[(i,  i) | i <- pos_range]]
      d2_winner = lookup_winners [[(i,2-i) | i <- pos_range]]
      
      lookup_winners :: [[Pos]] -> Maybe Player
      lookup_winners = msum . map lookup_winner
      
      lookup_winner :: [Pos] -> Maybe Player
      lookup_winner = list_winner . map (b `at`)
      
      list_winner xs = case nub xs of
        [maybe_p] -> maybe_p
        _         -> Nothing
  
  
  data GameIx TicTacTop2 = GameIx
     { overIx   :: Bool
     , minRowIx :: Int
     , playerIx :: PlayerIx
     , boardIx  :: BoardIx
     } deriving (Eq, Ord, Ix)
  
  game_range = (lo, hi)
    where
      lo = GameIx False 0 (fst player_range) (fst board_range)
      hi = GameIx True  2 (snd player_range) (snd board_range)
  
  game_index (TicTacTop2 o m p b) = GameIx
                                  { overIx   = o
                                  , minRowIx = m
                                  , playerIx = player_index p
                                  , boardIx  = board_index b
                                  }
  
  indexed_game (GameIx o m p b) = TicTacTop2
                                { over   = o
                                , minRow = m
                                , player = indexed_player p
                                , board  = indexed_board b
                                }
  
  
  newtype GameMove TicTacTop2 = GameMove
      { movePos :: Pos
      } deriving (Eq, Ord)
  
  legal_moves g = map GameMove $ filter legal positions
    where
      legal p = above_min p
             && unoccupied p
      unoccupied (1,1) | not (over g) = (board g `at` (1,1)) /= Just (player g)
      unoccupied p = (board g `at` p) == Nothing
      above_min p = snd p >= minRow g
  
  play g m = g''
    where
      (x, y) = movePos m
      y'  = y + 1
      y'' = if null (legal_moves g') then 0 else y'
      over' = board `at` (x,y) /= Nothing
      TicTacTop2 over minRow player board = g
      g'  = TicTacTop2 over' y' player' board'
      g'' = TicTacTop2 over' y'' player' board'
      player' = not player
      board'  = set_at (x, y) (Just player) board


instance AI TicTacTop2

instance Playable TicTacTop2 where
  markGame g = unlines . mark . lines where
    mark xs = zipWith (++) xs marks
    marks = replicate (minRow g) " (forbidden)" ++ repeat ""
  
  printGame = printBoard . board
  
  printChoices g = print_marked choiceBoard
    where
      print_marked = markGame g . unlines
      choiceBoard = (map.map) dot_if_illegal letterGrid
      dot_if_illegal c = if letterMove c `elem` legal_moves g
                           then c
                           else letterBoard `at` letterPos c
      letterBoard = (map.map) printCell (board g)
  
  validateChoice g [c] | c `elem` "123456789" = if letterMove c `elem` legal_moves g
                                                  then Just (letterMove c)
                                                  else Nothing
  validateChoice g _ = Nothing

letterGrid :: [[Char]]
letterGrid = ["123","456","789"]

letterPos :: Char -> Pos
letterPos = head . matching_positions
  where
    matching_positions c = filter (\p -> letterGrid `at` p == c) positions

letterMove :: Char -> GameMove TicTacTop2
letterMove = GameMove . letterPos
