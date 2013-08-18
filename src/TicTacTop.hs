{-# OPTIONS -XTypeFamilies #-}
module TicTacTop where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Function
import Data.List

import Player
import Board
import Game


data GameState = GameState
  { minRow :: Int
  , player :: Player
  , board :: Board
  }

instance Game GameState where
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
  
  
  data GameIx GameState = GameIx
     { minRowIx :: Int
     , playerIx :: PlayerIx
     , boardIx  :: BoardIx
     } deriving (Eq, Ord, Ix)
  
  game_range = (lo, hi)
    where
      lo = GameIx 0 (fst player_range) (fst board_range)
      hi = GameIx 2 (snd player_range) (snd board_range)
  
  game_index (GameState m p b) = GameIx
                               { minRowIx = m
                               , playerIx = player_index p
                               , boardIx  = board_index b
                               }
  
  indexed_game (GameIx m p b) = GameState
                              { minRow = m
                              , player = indexed_player p
                              , board  = indexed_board b
                              }
  
  
  newtype GameMove GameState = GameMove
      { movePos :: Pos
      } deriving (Eq, Ord)
  
  legal_moves g = map GameMove $ filter legal positions
    where
      legal p = unoccupied p
             && above_min p
      unoccupied p = (board g `at` p) == Nothing
      above_min p = snd p >= minRow g
  
  play g m = g''
    where
      (x, y) = movePos m
      y'  = y + 1
      y'' = if null (legal_moves g') then 0 else y'
      GameState minRow player board = g
      g'  = GameState y' player' board'
      g'' = GameState y'' player' board'
      player' = not player
      board'  = set_at (x, y) (Just player) board


best_moves :: Array (GameIx GameState) (BestMove GameState)
best_moves = fix mkBestMoves

best_move :: GameState -> BestMove GameState
best_move = (best_moves!) . game_index
