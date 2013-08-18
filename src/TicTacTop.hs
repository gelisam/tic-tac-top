{-# OPTIONS -XTypeFamilies -XFlexibleContexts -XScopedTypeVariables #-}
module TicTacTop where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Function
import Data.List

import Player
import Board
import Game


winner :: Board -> Winner
winner b = h_loser
       <|> v_winner
       <|> d1_winner
       <|> d2_winner
  where
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

tie :: GameState -> Bool
tie = null . legal_moves


data GameState = GameState
  { minRow :: Int
  , player :: Player
  , board :: Board
  }
type GameStateIx = (Int, PlayerIx, BoardIx)

game_range :: (GameStateIx, GameStateIx)
game_range = (lo, hi)
  where
    lo = (0, fst player_range, fst board_range)
    hi = (2, snd player_range, snd board_range)

game_index :: GameState -> GameStateIx
game_index (GameState m p b) = (m, player_index p, board_index b)

indexed_game :: GameStateIx -> GameState
indexed_game (m, p, b) = GameState m (indexed_player p) (indexed_board b)


type Move = Pos

legal_moves :: GameState -> [Move]
legal_moves g = filter legal positions
  where
    legal p = unoccupied p
           && above_min p
    unoccupied p = (board g `at` p) == Nothing
    above_min p = snd p >= minRow g

play :: GameState -> Move -> GameState
play (GameState minRow  player  board) (x, y) = g''
  where
    y'  = y + 1
    y'' = if null (legal_moves g') then 0 else y'
    g'  = GameState y' player' board'
    g'' = GameState y'' player' board'
    player' = not player
    board'  = set_at (x, y) (Just player) board


instance Game GameState where
  player' = player
  winner' = winner . board
  
  newtype GameIx GameState = GameStateIx'
     { runGameStateIx' :: GameStateIx
     } deriving (Eq, Ord, Ix)
  game_range' = let (x, y) = game_range
                 in (GameStateIx' x, GameStateIx' y)
  game_index' = GameStateIx' . game_index
  indexed_game' = indexed_game . runGameStateIx'
  
  newtype GameMove GameState = Move'
      { runMove' :: Move
      } deriving (Eq, Ord)
  legal_moves' = map Move' . legal_moves
  play' g = play g . runMove'

best_moves :: Array (GameIx GameState) (Maybe (GameMove GameState), Winner, Int)
best_moves = fix best_moves'

best_move :: GameState -> (Maybe Move, Winner, Int)
best_move = adjust . (best_moves!) . GameStateIx' . game_index
  where
    adjust (m, w, i) = (fmap runMove' m, w, i)
